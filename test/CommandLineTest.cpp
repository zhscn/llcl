//===- llcl/test/CommandLineTest.cpp - CommandLine tests
//--------------------===//
//
// Adapted from llvm/unittests/Support/CommandLineTest.cpp
// Standalone C++17 version - all LLVM types replaced with std:: equivalents.
//
//===----------------------------------------------------------------------===//

#include "llcl/CommandLine.h"

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <functional>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

#ifdef __unix__
#include <unistd.h>
#endif
#ifdef __APPLE__
#include <unistd.h>
#endif

using namespace llcl;

namespace {

//===----------------------------------------------------------------------===//
// Test Utilities
//===----------------------------------------------------------------------===//

/// Null output stream (replaces llvm::nulls())
class NullBuf : public std::streambuf {
protected:
  int_type overflow(int_type c) override { return c; }
};

class NullStream : public std::ostream {
  NullBuf buf;

public:
  NullStream() : std::ostream(&buf) {}
};

static NullStream &nulls() {
  static NullStream ns;
  return ns;
}

MATCHER(StringEquality, "Checks if two char* are equal as strings") {
  return std::string(std::get<0>(arg)) == std::string(std::get<1>(arg));
}

class TempEnvVar {
public:
  TempEnvVar(const char *name, const char *value) : name(name) {
    const char *old_value = getenv(name);
    EXPECT_EQ(nullptr, old_value) << old_value;
    setenv(name, value, true);
  }

  ~TempEnvVar() { unsetenv(name); }

private:
  const char *const name;
};

/// Simple TempDir using std::filesystem
class TempDir {
  std::filesystem::path DirPath;

public:
  TempDir(std::string_view Prefix, bool Unique) {
    if (Unique) {
      auto tmp = std::filesystem::temp_directory_path();
      std::string tmpl = (tmp / (std::string(Prefix) + "-XXXXXX")).string();
      if (mkdtemp(tmpl.data())) {
        DirPath = tmpl;
      }
    } else {
      DirPath = std::string(Prefix);
      std::filesystem::create_directories(DirPath);
    }
  }

  ~TempDir() {
    std::error_code EC;
    std::filesystem::remove_all(DirPath, EC);
  }

  std::string path() const { return DirPath.string(); }
  std::string path(std::string_view Component) const {
    return (DirPath / std::string(Component)).string();
  }
};

/// Simple TempFile using std::filesystem
class TempFile {
  std::filesystem::path FilePath_;

public:
  TempFile(std::string_view PathPrefix, std::string_view Ext,
           std::string_view Content, bool Unique = false) {
    if (Unique) {
      auto tmp = std::filesystem::temp_directory_path();
      std::string tmpl = (tmp / (std::string(PathPrefix) + "XXXXXX")).string();
      int fd = mkstemp(tmpl.data());
      if (fd >= 0) {
        ::close(fd);
        std::string finalPath = tmpl + std::string(Ext);
        if (!Ext.empty()) {
          std::rename(tmpl.c_str(), finalPath.c_str());
        } else {
          finalPath = tmpl;
        }
        {
          std::ofstream ofs(finalPath);
          ofs << Content;
        }
        FilePath_ = finalPath;
      }
    } else {
      std::string fullPath = std::string(PathPrefix) + std::string(Ext);
      {
        std::ofstream ofs(fullPath);
        ofs << Content;
      }
      FilePath_ = fullPath;
    }
  }

  ~TempFile() {
    std::error_code EC;
    std::filesystem::remove(FilePath_, EC);
  }

  std::string path() const { return FilePath_.string(); }
};

/// Captures stdout output during a function call
static std::string interceptStdout(std::function<void()> F) {
  std::cout.flush();
  fflush(stdout);

  auto tmpDir = std::filesystem::temp_directory_path();
  std::string tmpl = (tmpDir / "cli-stdout-XXXXXX").string();
  int newFd = mkstemp(tmpl.data());
  if (newFd == -1)
    return "";

  int oldFd = dup(STDOUT_FILENO);
  if (oldFd == -1) {
    ::close(newFd);
    std::filesystem::remove(tmpl);
    return "";
  }

  if (dup2(newFd, STDOUT_FILENO) == -1) {
    ::close(oldFd);
    ::close(newFd);
    std::filesystem::remove(tmpl);
    return "";
  }

  F();

  std::cout.flush();
  fflush(stdout);

  dup2(oldFd, STDOUT_FILENO);
  ::close(oldFd);
  ::close(newFd);

  std::ifstream ifs(tmpl);
  std::string result((std::istreambuf_iterator<char>(ifs)),
                     std::istreambuf_iterator<char>());

  std::filesystem::remove(tmpl);
  return result;
}

//===----------------------------------------------------------------------===//
// StackOption / StackSubCommand RAII wrappers
//===----------------------------------------------------------------------===//

template <typename T, typename Base = opt<T>> class StackOption : public Base {
public:
  template <class... Ts>
  explicit StackOption(Ts &&...Ms) : Base(std::forward<Ts>(Ms)...) {}

  ~StackOption() override { this->removeArgument(); }

  template <class DT> StackOption<T> &operator=(const DT &V) {
    Base::operator=(V);
    return *this;
  }
};

class StackSubCommand : public SubCommand {
public:
  StackSubCommand(std::string_view Name, std::string_view Description = {})
      : SubCommand(Name, Description) {}

  StackSubCommand() : SubCommand() {}

  ~StackSubCommand() { unregisterSubCommand(); }
};

//===----------------------------------------------------------------------===//
// Tests
//===----------------------------------------------------------------------===//

OptionCategory TestCategory("Test Options", "Description");

TEST(CommandLineTest, ModifyExisitingOption) {
  StackOption<int> TestOption("test-option", desc("old description"));

  static const char Description[] = "New description";
  static const char ArgString[] = "new-test-option";
  static const char ValueString[] = "Integer";

  auto &Map = getRegisteredOptions(SubCommand::getTopLevel());

  ASSERT_EQ(Map.count("test-option"), 1u) << "Could not find option in map.";

  Option *Retrieved = Map["test-option"];
  ASSERT_EQ(&TestOption, Retrieved) << "Retrieved wrong option.";

  ASSERT_NE(Retrieved->Categories.end(),
            std::find_if(Retrieved->Categories.begin(),
                         Retrieved->Categories.end(),
                         [&](const OptionCategory *Cat) {
                           return Cat == &getGeneralCategory();
                         }))
      << "Incorrect default option category.";

  Retrieved->addCategory(TestCategory);
  ASSERT_NE(Retrieved->Categories.end(),
            std::find_if(Retrieved->Categories.begin(),
                         Retrieved->Categories.end(),
                         [&](const OptionCategory *Cat) {
                           return Cat == &TestCategory;
                         }))
      << "Failed to modify option's option category.";

  Retrieved->setDescription(Description);
  ASSERT_STREQ(Retrieved->HelpStr.data(), Description)
      << "Changing option description failed.";

  Retrieved->setArgStr(ArgString);
  ASSERT_STREQ(ArgString, Retrieved->ArgStr.data())
      << "Failed to modify option's Argument string.";

  Retrieved->setValueStr(ValueString);
  ASSERT_STREQ(Retrieved->ValueStr.data(), ValueString)
      << "Failed to modify option's Value string.";

  Retrieved->setHiddenFlag(Hidden);
  ASSERT_EQ(Hidden, TestOption.getOptionHiddenFlag())
      << "Failed to modify option's hidden flag.";
}

TEST(CommandLineTest, UseOptionCategory) {
  StackOption<int> TestOption2("test-option", cat(TestCategory));

  ASSERT_NE(TestOption2.Categories.end(),
            std::find_if(TestOption2.Categories.begin(),
                         TestOption2.Categories.end(),
                         [&](const OptionCategory *Cat) {
                           return Cat == &TestCategory;
                         }))
      << "Failed to assign Option Category.";
}

TEST(CommandLineTest, UseMultipleCategories) {
  StackOption<int> TestOption2("test-option2", cat(TestCategory),
                               cat(getGeneralCategory()),
                               cat(getGeneralCategory()));

  // Make sure getGeneralCategory() wasn't added twice.
  ASSERT_EQ(TestOption2.Categories.size(), 2U);

  ASSERT_NE(TestOption2.Categories.end(),
            std::find_if(TestOption2.Categories.begin(),
                         TestOption2.Categories.end(),
                         [&](const OptionCategory *Cat) {
                           return Cat == &TestCategory;
                         }))
      << "Failed to assign Option Category.";
  ASSERT_NE(TestOption2.Categories.end(),
            std::find_if(TestOption2.Categories.begin(),
                         TestOption2.Categories.end(),
                         [&](const OptionCategory *Cat) {
                           return Cat == &getGeneralCategory();
                         }))
      << "Failed to assign General Category.";

  OptionCategory AnotherCategory("Additional test Options", "Description");
  StackOption<int> TestOption("test-option", cat(TestCategory),
                              cat(AnotherCategory));
  ASSERT_EQ(TestOption.Categories.end(),
            std::find_if(TestOption.Categories.begin(),
                         TestOption.Categories.end(),
                         [&](const OptionCategory *Cat) {
                           return Cat == &getGeneralCategory();
                         }))
      << "Failed to remove General Category.";
  ASSERT_NE(TestOption.Categories.end(),
            std::find_if(TestOption.Categories.begin(),
                         TestOption.Categories.end(),
                         [&](const OptionCategory *Cat) {
                           return Cat == &TestCategory;
                         }))
      << "Failed to assign Option Category.";
  ASSERT_NE(TestOption.Categories.end(),
            std::find_if(TestOption.Categories.begin(),
                         TestOption.Categories.end(),
                         [&](const OptionCategory *Cat) {
                           return Cat == &AnotherCategory;
                         }))
      << "Failed to assign Another Category.";
}

using ParserFunction = void(std::string_view Source, StringSaver &Saver,
                            std::vector<const char *> &NewArgv, bool MarkEOLs);

void testCommandLineTokenizer(ParserFunction *parse, std::string_view Input,
                              const std::vector<const char *> &Output,
                              bool MarkEOLs = false) {
  std::vector<const char *> Actual;
  BumpPtrAllocator A;
  StringSaver Saver(A);
  parse(Input, Saver, Actual, MarkEOLs);
  EXPECT_EQ(Output.size(), Actual.size());
  for (unsigned I = 0, E = Actual.size(); I != E; ++I) {
    if (I < Output.size()) {
      EXPECT_STREQ(Output[I], Actual[I]);
    }
  }
}

TEST(CommandLineTest, TokenizeGNUCommandLine) {
  const char Input[] =
      "foo\\ bar \"foo bar\" \'foo bar\' 'foo\\\\bar' -DFOO=bar\\(\\) "
      "foo\"bar\"baz C:\\\\src\\\\foo.cpp \"C:\\src\\foo.cpp\"";
  const std::vector<const char *> Output = {
      "foo bar",     "foo bar",   "foo bar",          "foo\\bar",
      "-DFOO=bar()", "foobarbaz", "C:\\src\\foo.cpp", "C:srcfoo.cpp"};
  testCommandLineTokenizer(TokenizeGNUCommandLine, Input, Output);
}

TEST(CommandLineTest, TokenizeWindowsCommandLine1) {
  const char Input[] =
      R"(a\b c\\d e\\"f g" h\"i j\\\"k "lmn" o pqr "st \"u" \v)";
  const std::vector<const char *> Output = {
      "a\\b", "c\\\\d", "e\\f g", "h\"i",   "j\\\"k",
      "lmn",  "o",      "pqr",    "st \"u", "\\v"};
  testCommandLineTokenizer(TokenizeWindowsCommandLine, Input, Output);
}

TEST(CommandLineTest, TokenizeWindowsCommandLine2) {
  const char Input[] = "clang -c -DFOO=\"\"\"ABC\"\"\" x.cpp";
  const std::vector<const char *> Output = {"clang", "-c", "-DFOO=\"ABC\"",
                                            "x.cpp"};
  testCommandLineTokenizer(TokenizeWindowsCommandLine, Input, Output);
}

TEST(CommandLineTest, TokenizeWindowsCommandLineQuotedLastArgument) {
  const char Input0[] = R"(a b c d )";
  const std::vector<const char *> Output0 = {"a", "b", "c", "d"};
  testCommandLineTokenizer(TokenizeWindowsCommandLine, Input0, Output0);

  const char Input1[] = R"(a b c d "")";
  const std::vector<const char *> Output1 = {"a", "b", "c", "d", ""};
  testCommandLineTokenizer(TokenizeWindowsCommandLine, Input1, Output1);

  const char Input2[] = R"(a b c d ")";
  const std::vector<const char *> Output2 = {"a", "b", "c", "d", ""};
  testCommandLineTokenizer(TokenizeWindowsCommandLine, Input2, Output2);
  const char Input3[] = R"(a b c d "text)";
  const std::vector<const char *> Output3 = {"a", "b", "c", "d", "text"};
  testCommandLineTokenizer(TokenizeWindowsCommandLine, Input3, Output3);
}

TEST(CommandLineTest, TokenizeWindowsCommandLineExeName) {
  const char Input1[] =
      R"("C:\Program Files\Whatever\"clang.exe z.c -DY=\"x\")";
  const std::vector<const char *> Output1 = {
      "C:\\Program Files\\Whatever\\clang.exe", "z.c", "-DY=\"x\""};
  testCommandLineTokenizer(TokenizeWindowsCommandLineFull, Input1, Output1);

  const char Input2[] = "\"a\\\"b c\\\"d\n\"e\\\"f g\\\"h\n";
  const std::vector<const char *> Output2 = {"a\\b", "c\"d", nullptr,
                                             "e\\f", "g\"h", nullptr};
  testCommandLineTokenizer(TokenizeWindowsCommandLineFull, Input2, Output2,
                           /*MarkEOLs=*/true);

  const char Input3[] = R"(\\server\share\subdir\clang.exe)";
  const std::vector<const char *> Output3 = {
      "\\\\server\\share\\subdir\\clang.exe"};
  testCommandLineTokenizer(TokenizeWindowsCommandLineFull, Input3, Output3);
}

TEST(CommandLineTest, TokenizeAndMarkEOLs) {
  const char Input[] = "clang -Xclang foo\n\nfoo\"bar\"baz\n x.cpp\n";
  const std::vector<const char *> Output = {"clang", "-Xclang", "foo",
                                            nullptr, nullptr,   "foobarbaz",
                                            nullptr, "x.cpp",   nullptr};
  testCommandLineTokenizer(TokenizeWindowsCommandLine, Input, Output,
                           /*MarkEOLs=*/true);
  testCommandLineTokenizer(TokenizeGNUCommandLine, Input, Output,
                           /*MarkEOLs=*/true);
}

TEST(CommandLineTest, TokenizeConfigFile1) {
  const char *Input = "\\";
  const std::vector<const char *> Output = {"\\"};
  testCommandLineTokenizer(tokenizeConfigFile, Input, Output);
}

TEST(CommandLineTest, TokenizeConfigFile2) {
  const char *Input = "\\abc";
  const std::vector<const char *> Output = {"abc"};
  testCommandLineTokenizer(tokenizeConfigFile, Input, Output);
}

TEST(CommandLineTest, TokenizeConfigFile3) {
  const char *Input = "abc\\";
  const std::vector<const char *> Output = {"abc\\"};
  testCommandLineTokenizer(tokenizeConfigFile, Input, Output);
}

TEST(CommandLineTest, TokenizeConfigFile4) {
  const char *Input = "abc\\\n123";
  const std::vector<const char *> Output = {"abc123"};
  testCommandLineTokenizer(tokenizeConfigFile, Input, Output);
}

TEST(CommandLineTest, TokenizeConfigFile5) {
  const char *Input = "abc\\\r\n123";
  const std::vector<const char *> Output = {"abc123"};
  testCommandLineTokenizer(tokenizeConfigFile, Input, Output);
}

TEST(CommandLineTest, TokenizeConfigFile6) {
  const char *Input = "abc\\\n";
  const std::vector<const char *> Output = {"abc"};
  testCommandLineTokenizer(tokenizeConfigFile, Input, Output);
}

TEST(CommandLineTest, TokenizeConfigFile7) {
  const char *Input = "abc\\\r\n";
  const std::vector<const char *> Output = {"abc"};
  testCommandLineTokenizer(tokenizeConfigFile, Input, Output);
}

TEST(CommandLineTest, TokenizeConfigFile8) {
  std::vector<const char *> Actual;
  BumpPtrAllocator A;
  StringSaver Saver(A);
  tokenizeConfigFile("\\\n", Saver, Actual, /*MarkEOLs=*/false);
  EXPECT_TRUE(Actual.empty());
}

TEST(CommandLineTest, TokenizeConfigFile9) {
  std::vector<const char *> Actual;
  BumpPtrAllocator A;
  StringSaver Saver(A);
  tokenizeConfigFile("\\\r\n", Saver, Actual, /*MarkEOLs=*/false);
  EXPECT_TRUE(Actual.empty());
}

TEST(CommandLineTest, TokenizeConfigFile10) {
  const char *Input = "\\\nabc";
  const std::vector<const char *> Output = {"abc"};
  testCommandLineTokenizer(tokenizeConfigFile, Input, Output);
}

TEST(CommandLineTest, TokenizeConfigFile11) {
  const char *Input = "\\\r\nabc";
  const std::vector<const char *> Output = {"abc"};
  testCommandLineTokenizer(tokenizeConfigFile, Input, Output);
}

TEST(CommandLineTest, AliasesWithArguments) {
  static const size_t ARGC = 3;
  const char *const Inputs[][ARGC] = {{"-tool", "-actual=x", "-extra"},
                                      {"-tool", "-actual", "x"},
                                      {"-tool", "-alias=x", "-extra"},
                                      {"-tool", "-alias", "x"}};

  for (size_t i = 0, e = std::size(Inputs); i < e; ++i) {
    StackOption<std::string> Actual("actual");
    StackOption<bool> Extra("extra");
    StackOption<std::string> Input(Positional);

    alias Alias("alias", aliasopt(Actual));

    ParseCommandLineOptions(ARGC, Inputs[i]);
    EXPECT_EQ("x", Actual);
    EXPECT_EQ(0, Input.getNumOccurrences());

    Alias.removeArgument();
  }
}

void testAliasRequired(int argc, const char *const *argv) {
  StackOption<std::string> Option("option", Required);
  alias Alias("o", aliasopt(Option));

  ParseCommandLineOptions(argc, argv);
  EXPECT_EQ("x", Option);
  EXPECT_EQ(1, Option.getNumOccurrences());

  Alias.removeArgument();
}

TEST(CommandLineTest, AliasRequired) {
  const char *opts1[] = {"-tool", "-option=x"};
  const char *opts2[] = {"-tool", "-o", "x"};
  testAliasRequired(std::size(opts1), opts1);
  testAliasRequired(std::size(opts2), opts2);
}

TEST(CommandLineTest, HideUnrelatedOptions) {
  StackOption<int> TestOption1("hide-option-1");
  StackOption<int> TestOption2("hide-option-2", cat(TestCategory));

  HideUnrelatedOptions(TestCategory);

  ASSERT_EQ(ReallyHidden, TestOption1.getOptionHiddenFlag())
      << "Failed to hide extra option.";
  ASSERT_EQ(NotHidden, TestOption2.getOptionHiddenFlag())
      << "Hid extra option that should be visable.";

  auto &Map = getRegisteredOptions(SubCommand::getTopLevel());
  ASSERT_TRUE(Map.count("help") == (size_t)0 ||
              NotHidden == Map["help"]->getOptionHiddenFlag())
      << "Hid default option that should be visable.";
}

OptionCategory TestCategory2("Test Options set 2", "Description");

TEST(CommandLineTest, HideUnrelatedOptionsMulti) {
  StackOption<int> TestOption1("multi-hide-option-1");
  StackOption<int> TestOption2("multi-hide-option-2", cat(TestCategory));
  StackOption<int> TestOption3("multi-hide-option-3", cat(TestCategory2));

  std::vector<const OptionCategory *> VisibleCategories = {&TestCategory,
                                                           &TestCategory2};

  HideUnrelatedOptions(VisibleCategories);

  ASSERT_EQ(ReallyHidden, TestOption1.getOptionHiddenFlag())
      << "Failed to hide extra option.";
  ASSERT_EQ(NotHidden, TestOption2.getOptionHiddenFlag())
      << "Hid extra option that should be visable.";
  ASSERT_EQ(NotHidden, TestOption3.getOptionHiddenFlag())
      << "Hid extra option that should be visable.";

  auto &Map = getRegisteredOptions(SubCommand::getTopLevel());
  ASSERT_TRUE(Map.count("help") == (size_t)0 ||
              NotHidden == Map["help"]->getOptionHiddenFlag())
      << "Hid default option that should be visable.";
}

TEST(CommandLineTest, SetMultiValues) {
  StackOption<int> Option("option");
  const char *args[] = {"prog", "-option=1", "-option=2"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args), args, "", &nulls()));
  EXPECT_EQ(Option, 2);
}

TEST(CommandLineTest, SetValueInSubcategories) {
  ResetCommandLineParser();

  StackSubCommand SC1("sc1", "First subcommand");
  StackSubCommand SC2("sc2", "Second subcommand");

  StackOption<bool> TopLevelOpt("top-level", init(false));
  StackOption<bool> SC1Opt("sc1", sub(SC1), init(false));
  StackOption<bool> SC2Opt("sc2", sub(SC2), init(false));

  EXPECT_FALSE(TopLevelOpt);
  EXPECT_FALSE(SC1Opt);
  EXPECT_FALSE(SC2Opt);
  const char *args[] = {"prog", "-top-level"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args, "", &nulls()));
  EXPECT_TRUE(TopLevelOpt);
  EXPECT_FALSE(SC1Opt);
  EXPECT_FALSE(SC2Opt);

  TopLevelOpt = false;

  ResetAllOptionOccurrences();
  EXPECT_FALSE(TopLevelOpt);
  EXPECT_FALSE(SC1Opt);
  EXPECT_FALSE(SC2Opt);
  const char *args2[] = {"prog", "sc1", "-sc1"};
  EXPECT_TRUE(ParseCommandLineOptions(3, args2, "", &nulls()));
  EXPECT_FALSE(TopLevelOpt);
  EXPECT_TRUE(SC1Opt);
  EXPECT_FALSE(SC2Opt);

  SC1Opt = false;

  ResetAllOptionOccurrences();
  EXPECT_FALSE(TopLevelOpt);
  EXPECT_FALSE(SC1Opt);
  EXPECT_FALSE(SC2Opt);
  const char *args3[] = {"prog", "sc2", "-sc2"};
  EXPECT_TRUE(ParseCommandLineOptions(3, args3, "", &nulls()));
  EXPECT_FALSE(TopLevelOpt);
  EXPECT_FALSE(SC1Opt);
  EXPECT_TRUE(SC2Opt);
}

TEST(CommandLineTest, LookupFailsInWrongSubCommand) {
  ResetCommandLineParser();

  StackSubCommand SC1("sc1", "First subcommand");
  StackSubCommand SC2("sc2", "Second subcommand");

  StackOption<bool> SC1Opt("sc1", sub(SC1), init(false));
  StackOption<bool> SC2Opt("sc2", sub(SC2), init(false));

  std::ostringstream OS;

  const char *args[] = {"prog", "sc1", "-sc2"};
  EXPECT_FALSE(ParseCommandLineOptions(3, args, "", &OS));
  EXPECT_FALSE(OS.str().empty());
}

TEST(CommandLineTest, TopLevelOptInSubcommand) {
  enum LiteralOptionEnum {
    foo,
    bar,
    baz,
  };

  ResetCommandLineParser();

  StackOption<std::string> TopLevelOpt("str", init("txt"),
                                       desc("A top-level option."));

  StackSubCommand SC("sc", "Subcommand");
  StackOption<std::string> PositionalOpt(
      Positional, desc("positional argument test coverage"), sub(SC));
  StackOption<LiteralOptionEnum> LiteralOpt(
      desc("literal argument test coverage"), sub(SC), init(bar),
      values(clEnumVal(foo, "foo"), clEnumVal(bar, "bar"),
             clEnumVal(baz, "baz")));
  StackOption<bool> EnableOpt("enable", sub(SC), init(false));
  StackOption<int> ThresholdOpt("threshold", sub(SC), init(1));

  const char *PositionalOptVal = "input-file";
  const char *args[] = {"prog",    "sc",        PositionalOptVal,
                        "-enable", "--str=csv", "--threshold=2"};

  ASSERT_TRUE(
      ParseCommandLineOptions(sizeof(args) / sizeof(args[0]), args, ""));
  EXPECT_STREQ(PositionalOpt.getValue().c_str(), PositionalOptVal);
  EXPECT_TRUE(EnableOpt);
  EXPECT_STREQ(TopLevelOpt.getValue().c_str(), "csv");
  EXPECT_EQ(ThresholdOpt, 2);

  for (auto &[LiteralOptVal, WantLiteralOpt] :
       {std::pair{"--bar", bar}, {"--foo", foo}, {"--baz", baz}}) {
    const char *args[] = {"prog", "sc", LiteralOptVal};
    ASSERT_TRUE(
        ParseCommandLineOptions(sizeof(args) / sizeof(args[0]), args, ""));
    EXPECT_EQ(LiteralOpt, WantLiteralOpt);
  }
}

TEST(CommandLineTest, AddToAllSubCommands) {
  ResetCommandLineParser();

  StackSubCommand SC1("sc1", "First subcommand");
  StackOption<bool> AllOpt("everywhere", sub(SubCommand::getAll()),
                           init(false));
  StackSubCommand SC2("sc2", "Second subcommand");

  EXPECT_TRUE(SubCommand::getTopLevel().OptionsMap.count("everywhere") > 0);
  EXPECT_TRUE(SubCommand::getAll().OptionsMap.count("everywhere") > 0);
  EXPECT_TRUE(SC1.OptionsMap.count("everywhere") > 0);
  EXPECT_TRUE(SC2.OptionsMap.count("everywhere") > 0);

  const char *args[] = {"prog", "-everywhere"};
  const char *args2[] = {"prog", "sc1", "-everywhere"};
  const char *args3[] = {"prog", "sc2", "-everywhere"};

  std::ostringstream OS;

  EXPECT_FALSE(AllOpt);
  EXPECT_TRUE(ParseCommandLineOptions(2, args, "", &OS));
  EXPECT_TRUE(AllOpt);

  AllOpt = false;

  ResetAllOptionOccurrences();
  EXPECT_FALSE(AllOpt);
  EXPECT_TRUE(ParseCommandLineOptions(3, args2, "", &OS));
  EXPECT_TRUE(AllOpt);

  AllOpt = false;

  ResetAllOptionOccurrences();
  EXPECT_FALSE(AllOpt);
  EXPECT_TRUE(ParseCommandLineOptions(3, args3, "", &OS));
  EXPECT_TRUE(AllOpt);

  EXPECT_TRUE(OS.str().empty());
}

TEST(CommandLineTest, ReparseCommandLineOptions) {
  ResetCommandLineParser();

  StackOption<bool> TopLevelOpt("top-level", sub(SubCommand::getTopLevel()),
                                init(false));

  const char *args[] = {"prog", "-top-level"};

  EXPECT_FALSE(TopLevelOpt);
  EXPECT_TRUE(ParseCommandLineOptions(2, args, "", &nulls()));
  EXPECT_TRUE(TopLevelOpt);

  TopLevelOpt = false;

  ResetAllOptionOccurrences();
  EXPECT_FALSE(TopLevelOpt);
  EXPECT_TRUE(ParseCommandLineOptions(2, args, "", &nulls()));
  EXPECT_TRUE(TopLevelOpt);
}

TEST(CommandLineTest, RemoveFromRegularSubCommand) {
  ResetCommandLineParser();

  StackSubCommand SC("sc", "Subcommand");
  StackOption<bool> RemoveOption("remove-option", sub(SC), init(false));
  StackOption<bool> KeepOption("keep-option", sub(SC), init(false));

  const char *args[] = {"prog", "sc", "-remove-option"};

  std::ostringstream OS;

  EXPECT_FALSE(RemoveOption);
  EXPECT_TRUE(ParseCommandLineOptions(3, args, "", &OS));
  EXPECT_TRUE(RemoveOption);
  EXPECT_TRUE(OS.str().empty());

  RemoveOption.removeArgument();

  ResetAllOptionOccurrences();
  OS.str("");
  OS.clear();
  EXPECT_FALSE(ParseCommandLineOptions(3, args, "", &OS));
  EXPECT_FALSE(OS.str().empty());
}

TEST(CommandLineTest, RemoveFromTopLevelSubCommand) {
  ResetCommandLineParser();

  StackOption<bool> TopLevelRemove("top-level-remove",
                                   sub(SubCommand::getTopLevel()), init(false));
  StackOption<bool> TopLevelKeep("top-level-keep",
                                 sub(SubCommand::getTopLevel()), init(false));

  const char *args[] = {"prog", "-top-level-remove"};

  EXPECT_FALSE(TopLevelRemove);
  EXPECT_TRUE(ParseCommandLineOptions(2, args, "", &nulls()));
  EXPECT_TRUE(TopLevelRemove);

  TopLevelRemove.removeArgument();

  ResetAllOptionOccurrences();
  EXPECT_FALSE(ParseCommandLineOptions(2, args, "", &nulls()));
}

TEST(CommandLineTest, RemoveFromAllSubCommands) {
  ResetCommandLineParser();

  StackSubCommand SC1("sc1", "First Subcommand");
  StackSubCommand SC2("sc2", "Second Subcommand");
  StackOption<bool> RemoveOption("remove-option", sub(SubCommand::getAll()),
                                 init(false));
  StackOption<bool> KeepOption("keep-option", sub(SubCommand::getAll()),
                               init(false));

  const char *args0[] = {"prog", "-remove-option"};
  const char *args1[] = {"prog", "sc1", "-remove-option"};
  const char *args2[] = {"prog", "sc2", "-remove-option"};

  EXPECT_FALSE(RemoveOption);
  EXPECT_TRUE(ParseCommandLineOptions(2, args0, "", &nulls()));
  EXPECT_TRUE(RemoveOption);

  RemoveOption = false;

  ResetAllOptionOccurrences();
  EXPECT_FALSE(RemoveOption);
  EXPECT_TRUE(ParseCommandLineOptions(3, args1, "", &nulls()));
  EXPECT_TRUE(RemoveOption);

  RemoveOption = false;

  ResetAllOptionOccurrences();
  EXPECT_FALSE(RemoveOption);
  EXPECT_TRUE(ParseCommandLineOptions(3, args2, "", &nulls()));
  EXPECT_TRUE(RemoveOption);

  RemoveOption.removeArgument();

  ResetAllOptionOccurrences();
  EXPECT_FALSE(ParseCommandLineOptions(2, args0, "", &nulls()));
  ResetAllOptionOccurrences();
  EXPECT_FALSE(ParseCommandLineOptions(3, args1, "", &nulls()));
  ResetAllOptionOccurrences();
  EXPECT_FALSE(ParseCommandLineOptions(3, args2, "", &nulls()));
}

TEST(CommandLineTest, GetRegisteredSubcommands) {
  ResetCommandLineParser();

  StackSubCommand SC1("sc1", "First Subcommand");
  StackOption<bool> Opt1("opt1", sub(SC1), init(false));
  StackSubCommand SC2("sc2", "Second subcommand");
  StackOption<bool> Opt2("opt2", sub(SC2), init(false));

  const char *args0[] = {"prog", "sc1"};
  const char *args1[] = {"prog", "sc2"};

  EXPECT_TRUE(ParseCommandLineOptions(2, args0, "", &nulls()));
  EXPECT_FALSE(Opt1);
  EXPECT_FALSE(Opt2);
  for (auto *S : getRegisteredSubcommands()) {
    if (*S) {
      EXPECT_EQ("sc1", S->getName());
    }
  }

  ResetAllOptionOccurrences();
  EXPECT_TRUE(ParseCommandLineOptions(2, args1, "", &nulls()));
  EXPECT_FALSE(Opt1);
  EXPECT_FALSE(Opt2);
  for (auto *S : getRegisteredSubcommands()) {
    if (*S) {
      EXPECT_EQ("sc2", S->getName());
    }
  }
}

TEST(CommandLineTest, DefaultOptions) {
  ResetCommandLineParser();

  StackOption<std::string> Bar("bar", sub(SubCommand::getAll()), DefaultOption);
  StackOption<bool, alias> Bar_Alias("b", desc("Alias for -bar"), aliasopt(Bar),
                                     DefaultOption);

  StackOption<bool> Foo("foo", init(false), sub(SubCommand::getAll()),
                        DefaultOption);
  StackOption<bool, alias> Foo_Alias("f", desc("Alias for -foo"), aliasopt(Foo),
                                     DefaultOption);

  StackSubCommand SC1("sc1", "First Subcommand");
  StackOption<bool> SC1_B("b", sub(SC1), init(false));
  StackSubCommand SC2("sc2", "Second subcommand");
  StackOption<std::string> SC2_Foo("foo", sub(SC2));

  const char *args0[] = {"prog", "-b", "args0 bar string", "-f"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args0), args0, "", &nulls()));
  EXPECT_EQ(Bar, "args0 bar string");
  EXPECT_TRUE(Foo);
  EXPECT_FALSE(SC1_B);
  EXPECT_TRUE(SC2_Foo.empty());

  ResetAllOptionOccurrences();

  const char *args1[] = {"prog", "sc1", "-b", "-bar", "args1 bar string", "-f"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args1), args1, "", &nulls()));
  EXPECT_EQ(Bar, "args1 bar string");
  EXPECT_TRUE(Foo);
  EXPECT_TRUE(SC1_B);
  EXPECT_TRUE(SC2_Foo.empty());
  for (auto *S : getRegisteredSubcommands()) {
    if (*S) {
      EXPECT_EQ("sc1", S->getName());
    }
  }

  ResetAllOptionOccurrences();

  const char *args2[] = {"prog", "sc2",  "-b",        "args2 bar string",
                         "-f",   "-foo", "foo string"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args2), args2, "", &nulls()));
  EXPECT_EQ(Bar, "args2 bar string");
  EXPECT_TRUE(Foo);
  EXPECT_FALSE(SC1_B);
  EXPECT_EQ(SC2_Foo, "foo string");
  for (auto *S : getRegisteredSubcommands()) {
    if (*S) {
      EXPECT_EQ("sc2", S->getName());
    }
  }
  ResetCommandLineParser();
}

TEST(CommandLineTest, SetDefaultValue) {
  ResetCommandLineParser();

  StackOption<std::string> Opt1("opt1", init("true"));
  StackOption<bool> Opt2("opt2", init(true));
  alias Alias("alias", aliasopt(Opt2));
  StackOption<int> Opt3("opt3", init(3));

  std::vector<int> IntVals = {1, 2, 3};
  std::vector<std::string> StrVals = {"foo", "bar", "baz"};

  StackOption<int, list<int>> List1("list1", list_init<int>(IntVals),
                                    CommaSeparated);
  StackOption<std::string, list<std::string>> List2(
      "list2", list_init<std::string>(StrVals), CommaSeparated);
  alias ListAlias("list-alias", aliasopt(List2));

  const char *args[] = {"prog",   "-opt1=false", "-list1", "4",
                        "-list1", "5,6",         "-opt2",  "-opt3"};

  EXPECT_TRUE(ParseCommandLineOptions(7, args, "", &nulls()));

  EXPECT_EQ(Opt1, "false");
  EXPECT_TRUE(Opt2);
  EXPECT_EQ(Opt3, 3);

  for (size_t I = 0, E = IntVals.size(); I < E; ++I) {
    EXPECT_EQ(IntVals[I] + 3, List1[I]);
    EXPECT_EQ(StrVals[I], List2[I]);
  }

  Opt2 = false;
  Opt3 = 1;

  ResetAllOptionOccurrences();

  for (auto &OM : getRegisteredOptions(SubCommand::getTopLevel())) {
    Option *O = OM.second;
    if (O->ArgStr == "opt2") {
      continue;
    }
    O->setDefault();
  }

  EXPECT_EQ(Opt1, "true");
  EXPECT_TRUE(Opt2);
  EXPECT_EQ(Opt3, 3);
  for (size_t I = 0, E = IntVals.size(); I < E; ++I) {
    EXPECT_EQ(IntVals[I], List1[I]);
    EXPECT_EQ(StrVals[I], List2[I]);
  }

  Alias.removeArgument();
  ListAlias.removeArgument();
}

TEST(CommandLineTest, ReadConfigFile) {
  std::vector<const char *> Argv;

  TempDir TestDir("unittest", /*Unique*/ true);
  TempDir TestSubDir(TestDir.path("subdir"), /*Unique*/ false);

  std::string TestCfg = TestDir.path("foo");
  TempFile ConfigFile(TestCfg, "",
                      "# Comment\n"
                      "-option_1\n"
                      "-option_2=<CFGDIR>/dir1\n"
                      "-option_3=<CFGDIR>\n"
                      "-option_4 <CFGDIR>\n"
                      "-option_5=<CFG\\\n"
                      "DIR>\n"
                      "-option_6=<CFGDIR>/dir1,<CFGDIR>/dir2\n"
                      "@subconfig\n"
                      "-option_11=abcd\n"
                      "-option_12=\\\n"
                      "cdef\n");

  std::string TestCfg2 = TestDir.path("subconfig");
  TempFile ConfigFile2(TestCfg2, "",
                       "-option_7\n"
                       "-option_8=<CFGDIR>/dir2\n"
                       "@subdir/subfoo\n"
                       "\n"
                       "   # comment\n");

  std::string TestCfg3 = TestSubDir.path("subfoo");
  TempFile ConfigFile3(TestCfg3, "",
                       "-option_9=<CFGDIR>/dir3\n"
                       "@<CFGDIR>/subfoo2\n");

  std::string TestCfg4 = TestSubDir.path("subfoo2");
  TempFile ConfigFile4(TestCfg4, "", "-option_10\n");

  auto CurrDir = std::filesystem::current_path();
  EXPECT_NE(CurrDir.string(), TestDir.path());

  BumpPtrAllocator A;
  ExpansionContext ECtx(A, tokenizeConfigFile);
  llcl::Error Result = ECtx.readConfigFile(ConfigFile.path(), Argv);

  EXPECT_FALSE((bool)Result);
  EXPECT_EQ(Argv.size(), 13U);
  EXPECT_STREQ(Argv[0], "-option_1");
  EXPECT_STREQ(Argv[1], ("-option_2=" + TestDir.path() + "/dir1").c_str());
  EXPECT_STREQ(Argv[2], ("-option_3=" + TestDir.path()).c_str());
  EXPECT_STREQ(Argv[3], "-option_4");
  EXPECT_STREQ(Argv[4], TestDir.path().c_str());
  EXPECT_STREQ(Argv[5], ("-option_5=" + TestDir.path()).c_str());
  EXPECT_STREQ(Argv[6], ("-option_6=" + TestDir.path() + "/dir1," +
                         TestDir.path() + "/dir2")
                            .c_str());
  EXPECT_STREQ(Argv[7], "-option_7");
  EXPECT_STREQ(Argv[8], ("-option_8=" + TestDir.path() + "/dir2").c_str());
  EXPECT_STREQ(Argv[9], ("-option_9=" + TestSubDir.path() + "/dir3").c_str());
  EXPECT_STREQ(Argv[10], "-option_10");
  EXPECT_STREQ(Argv[11], "-option_11=abcd");
  EXPECT_STREQ(Argv[12], "-option_12=cdef");
}

TEST(CommandLineTest, PositionalEatArgsError) {
  ResetCommandLineParser();

  StackOption<std::string, list<std::string>> PosEatArgs(
      "positional-eat-args", Positional, desc("<arguments>..."),
      PositionalEatsArgs);
  StackOption<std::string, list<std::string>> PosEatArgs2(
      "positional-eat-args2", Positional, desc("Some strings"),
      PositionalEatsArgs);

  const char *args[] = {"prog", "-positional-eat-args=XXXX"};
  const char *args2[] = {"prog", "-positional-eat-args=XXXX", "-foo"};
  const char *args3[] = {"prog", "-positional-eat-args", "-foo"};
  const char *args4[] = {"prog", "-positional-eat-args",
                         "-foo", "-positional-eat-args2",
                         "-bar", "foo"};

  std::ostringstream OS;
  EXPECT_FALSE(ParseCommandLineOptions(2, args, "", &OS));
  EXPECT_FALSE(OS.str().empty());
  OS.str("");
  OS.clear();
  EXPECT_FALSE(ParseCommandLineOptions(3, args2, "", &OS));
  EXPECT_FALSE(OS.str().empty());
  OS.str("");
  OS.clear();
  EXPECT_TRUE(ParseCommandLineOptions(3, args3, "", &OS));
  EXPECT_TRUE(OS.str().empty());
  OS.str("");
  OS.clear();

  ResetAllOptionOccurrences();
  EXPECT_TRUE(ParseCommandLineOptions(6, args4, "", &OS));
  EXPECT_EQ(PosEatArgs.size(), 1u);
  EXPECT_EQ(PosEatArgs2.size(), 2u);
  EXPECT_TRUE(OS.str().empty());
}

TEST(CommandLineTest, BadResponseFile) {
  BumpPtrAllocator A;
  StringSaver Saver(A);
  TempDir ADir("dir", /*Unique*/ true);
  std::string AFilePath =
      (std::filesystem::path(ADir.path()) / "file.rsp").string();
  std::string AFileExp = std::string("@") + AFilePath;
  std::vector<const char *> Argv = {"clang", AFileExp.c_str()};

  bool Res = ExpandResponseFiles(Saver, TokenizeGNUCommandLine, Argv);
  ASSERT_TRUE(Res);
  ASSERT_EQ(2U, Argv.size());
  ASSERT_STREQ(Argv[0], "clang");
  ASSERT_STREQ(Argv[1], AFileExp.c_str());

  std::string ADirExp = std::string("@") + ADir.path();
  Argv = {"clang", ADirExp.c_str()};
  Res = ExpandResponseFiles(Saver, TokenizeGNUCommandLine, Argv);
  ASSERT_FALSE(Res);
  ASSERT_EQ(2U, Argv.size());
  ASSERT_STREQ(Argv[0], "clang");
  ASSERT_STREQ(Argv[1], ADirExp.c_str());
}

TEST(CommandLineTest, PrefixOptions) {
  ResetCommandLineParser();

  StackOption<std::string, list<std::string>> IncludeDirs(
      "I", Prefix, desc("Declare an include directory"));

  EXPECT_TRUE(IncludeDirs.empty());
  const char *args[] = {"prog", "-I=/usr/include"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args, "", &nulls()));
  EXPECT_EQ(IncludeDirs.size(), 1u);
  EXPECT_EQ(IncludeDirs.front().compare("/usr/include"), 0);

  IncludeDirs.erase(IncludeDirs.begin());
  ResetAllOptionOccurrences();

  EXPECT_TRUE(IncludeDirs.empty());
  const char *args2[] = {"prog", "-I", "/usr/include"};
  EXPECT_TRUE(ParseCommandLineOptions(3, args2, "", &nulls()));
  EXPECT_EQ(IncludeDirs.size(), 1u);
  EXPECT_EQ(IncludeDirs.front().compare("/usr/include"), 0);

  IncludeDirs.erase(IncludeDirs.begin());
  ResetAllOptionOccurrences();

  EXPECT_TRUE(IncludeDirs.empty());
  const char *args3[] = {"prog", "-I/usr/include"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args3, "", &nulls()));
  EXPECT_EQ(IncludeDirs.size(), 1u);
  EXPECT_EQ(IncludeDirs.front().compare("/usr/include"), 0);

  StackOption<std::string, list<std::string>> MacroDefs(
      "D", AlwaysPrefix, desc("Define a macro"), value_desc("MACRO[=VALUE]"));

  ResetAllOptionOccurrences();

  EXPECT_TRUE(MacroDefs.empty());
  const char *args4[] = {"prog", "-D=HAVE_FOO"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args4, "", &nulls()));
  EXPECT_EQ(MacroDefs.size(), 1u);
  EXPECT_EQ(MacroDefs.front().compare("=HAVE_FOO"), 0);

  MacroDefs.erase(MacroDefs.begin());
  ResetAllOptionOccurrences();

  EXPECT_TRUE(MacroDefs.empty());
  const char *args5[] = {"prog", "-D", "HAVE_FOO"};
  EXPECT_FALSE(ParseCommandLineOptions(3, args5, "", &nulls()));
  EXPECT_TRUE(MacroDefs.empty());

  ResetAllOptionOccurrences();

  EXPECT_TRUE(MacroDefs.empty());
  const char *args6[] = {"prog", "-DHAVE_FOO"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args6, "", &nulls()));
  EXPECT_EQ(MacroDefs.size(), 1u);
  EXPECT_EQ(MacroDefs.front().compare("HAVE_FOO"), 0);
}

TEST(CommandLineTest, GroupingWithValue) {
  ResetCommandLineParser();

  StackOption<bool> OptF("f", Grouping, desc("Some flag"));
  StackOption<bool> OptB("b", Grouping, desc("Another flag"));
  StackOption<bool> OptD("d", Grouping, ValueDisallowed,
                         desc("ValueDisallowed option"));
  StackOption<std::string> OptV("v", Grouping, desc("ValueRequired option"));
  StackOption<std::string> OptO("o", Grouping, ValueOptional,
                                desc("ValueOptional option"));

  const char *args1[] = {"prog", "-fv", "val1"};
  EXPECT_TRUE(ParseCommandLineOptions(3, args1, "", &nulls()));
  EXPECT_TRUE(OptF);
  EXPECT_STREQ("val1", OptV.c_str());
  OptV.clear();
  ResetAllOptionOccurrences();

  const char *args2[] = {"prog", "-vf", "val2"};
  EXPECT_FALSE(ParseCommandLineOptions(3, args2, "", &nulls()));
  OptV.clear();
  ResetAllOptionOccurrences();

  const char *args3[] = {"prog", "-fv=val3"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args3, "", &nulls()));
  EXPECT_TRUE(OptF);
  EXPECT_STREQ("val3", OptV.c_str());
  OptV.clear();
  ResetAllOptionOccurrences();

  const char *args4[] = {"prog", "-fo=val4"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args4, "", &nulls()));
  EXPECT_TRUE(OptF);
  EXPECT_STREQ("val4", OptO.c_str());
  OptO.clear();
  ResetAllOptionOccurrences();

  const char *args5[] = {"prog", "-fob"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args5, "", &nulls()));
  EXPECT_TRUE(OptF);
  EXPECT_EQ(1, OptO.getNumOccurrences());
  EXPECT_EQ(1, OptB.getNumOccurrences());
  EXPECT_TRUE(OptO.empty());
  ResetAllOptionOccurrences();

  const char *args6[] = {"prog", "-fd=false"};
  EXPECT_FALSE(ParseCommandLineOptions(2, args6, "", &nulls()));
}

TEST(CommandLineTest, GroupingAndPrefix) {
  ResetCommandLineParser();

  StackOption<bool> OptF("f", Grouping, desc("Some flag"));
  StackOption<bool> OptB("b", Grouping, desc("Another flag"));
  StackOption<std::string> OptP("p", Prefix, Grouping,
                                desc("Prefix and Grouping"));
  StackOption<std::string> OptA("a", AlwaysPrefix, Grouping,
                                desc("AlwaysPrefix and Grouping"));

  const char *args1[] = {"prog", "-pval1"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args1, "", &nulls()));
  EXPECT_STREQ("val1", OptP.c_str());
  OptP.clear();
  ResetAllOptionOccurrences();

  const char *args2[] = {"prog", "-p", "val2"};
  EXPECT_TRUE(ParseCommandLineOptions(3, args2, "", &nulls()));
  EXPECT_STREQ("val2", OptP.c_str());
  OptP.clear();
  ResetAllOptionOccurrences();

  const char *args3[] = {"prog", "-p=val3"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args3, "", &nulls()));
  EXPECT_STREQ("val3", OptP.c_str());
  OptP.clear();
  ResetAllOptionOccurrences();

  const char *args4[] = {"prog", "-fpval4"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args4, "", &nulls()));
  EXPECT_TRUE(OptF);
  EXPECT_STREQ("val4", OptP.c_str());
  OptP.clear();
  ResetAllOptionOccurrences();

  const char *args5[] = {"prog", "-fp", "val5"};
  EXPECT_TRUE(ParseCommandLineOptions(3, args5, "", &nulls()));
  EXPECT_TRUE(OptF);
  EXPECT_STREQ("val5", OptP.c_str());
  OptP.clear();
  ResetAllOptionOccurrences();

  const char *args6[] = {"prog", "-fp=val6"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args6, "", &nulls()));
  EXPECT_TRUE(OptF);
  EXPECT_STREQ("val6", OptP.c_str());
  OptP.clear();
  ResetAllOptionOccurrences();

  const char *args7[] = {"prog", "-fpb"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args7, "", &nulls()));
  EXPECT_TRUE(OptF);
  EXPECT_STREQ("b", OptP.c_str());
  EXPECT_FALSE(OptB);
  OptP.clear();
  ResetAllOptionOccurrences();

  const char *args8[] = {"prog", "-aval8"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args8, "", &nulls()));
  EXPECT_STREQ("val8", OptA.c_str());
  OptA.clear();
  ResetAllOptionOccurrences();

  const char *args9[] = {"prog", "-a", "val9"};
  EXPECT_FALSE(ParseCommandLineOptions(3, args9, "", &nulls()));
  ResetAllOptionOccurrences();

  const char *args10[] = {"prog", "-a=val10"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args10, "", &nulls()));
  EXPECT_STREQ("=val10", OptA.c_str());
  OptA.clear();
  ResetAllOptionOccurrences();

  const char *args11[] = {"prog", "-faval11"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args11, "", &nulls()));
  EXPECT_TRUE(OptF);
  EXPECT_STREQ("val11", OptA.c_str());
  OptA.clear();
  ResetAllOptionOccurrences();

  const char *args12[] = {"prog", "-fa", "val12"};
  EXPECT_FALSE(ParseCommandLineOptions(3, args12, "", &nulls()));
  ResetAllOptionOccurrences();

  const char *args13[] = {"prog", "-fa=val13"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args13, "", &nulls()));
  EXPECT_TRUE(OptF);
  EXPECT_STREQ("=val13", OptA.c_str());
  OptA.clear();
  ResetAllOptionOccurrences();

  const char *args14[] = {"prog", "-fab"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args14, "", &nulls()));
  EXPECT_TRUE(OptF);
  EXPECT_STREQ("b", OptA.c_str());
  EXPECT_FALSE(OptB);
  OptA.clear();
  ResetAllOptionOccurrences();
}

TEST(CommandLineTest, LongOptions) {
  ResetCommandLineParser();

  StackOption<bool> OptA("a", desc("Some flag"));
  StackOption<bool> OptBLong("long-flag", desc("Some long flag"));
  StackOption<bool, alias> OptB("b", desc("Alias to --long-flag"),
                                aliasopt(OptBLong));
  StackOption<std::string> OptAB("ab", desc("Another long option"));

  std::ostringstream OS;

  const char *args1[] = {"prog", "-a", "-ab", "val1"};
  const char *args2[] = {"prog", "-a", "--ab", "val1"};
  const char *args3[] = {"prog", "-ab", "--ab", "val1"};

  EXPECT_TRUE(ParseCommandLineOptions(4, args1, "", &OS));
  EXPECT_TRUE(OptA);
  EXPECT_FALSE(OptBLong);
  EXPECT_STREQ("val1", OptAB.c_str());
  EXPECT_TRUE(OS.str().empty());
  OS.str("");
  OS.clear();
  ResetAllOptionOccurrences();

  EXPECT_TRUE(ParseCommandLineOptions(4, args2, "", &OS));
  EXPECT_TRUE(OptA);
  EXPECT_FALSE(OptBLong);
  EXPECT_STREQ("val1", OptAB.c_str());
  EXPECT_TRUE(OS.str().empty());
  OS.str("");
  OS.clear();
  ResetAllOptionOccurrences();

  EXPECT_FALSE(ParseCommandLineOptions(4, args3, "", &OS));
  std::cout << OS.str() << "\n";
  EXPECT_FALSE(OS.str().empty());
  OS.str("");
  OS.clear();
  ResetAllOptionOccurrences();

  // LongOptionsUseDoubleDash = true
  EXPECT_FALSE(ParseCommandLineOptions(4, args1, "", &OS, nullptr, true));
  EXPECT_FALSE(OS.str().empty());
  OS.str("");
  OS.clear();
  ResetAllOptionOccurrences();

  EXPECT_TRUE(ParseCommandLineOptions(4, args2, "", &OS, nullptr, true));
  EXPECT_TRUE(OS.str().empty());
  OS.str("");
  OS.clear();
  ResetAllOptionOccurrences();

  EXPECT_TRUE(ParseCommandLineOptions(4, args3, "", &OS, nullptr, true));
  EXPECT_TRUE(OptA);
  EXPECT_TRUE(OptBLong);
  EXPECT_STREQ("val1", OptAB.c_str());
  EXPECT_TRUE(OS.str().empty());
  OS.str("");
  OS.clear();
  ResetAllOptionOccurrences();
}

TEST(CommandLineTest, OptionErrorMessage) {
  ResetCommandLineParser();

  StackOption<bool> OptA("a", desc("Some option"));
  StackOption<bool> OptLong("long", desc("Some long option"));

  std::ostringstream OS;

  OptA.error("custom error", OS);
  EXPECT_NE(OS.str().find("for the -a option:"), std::string::npos);
  OS.str("");
  OS.clear();

  OptLong.error("custom error", OS);
  EXPECT_NE(OS.str().find("for the --long option:"), std::string::npos);
  OS.str("");
  OS.clear();

  ResetAllOptionOccurrences();
}

TEST(CommandLineTest, OptionErrorMessageSuggest) {
  ResetCommandLineParser();

  StackOption<bool> OptLong("aluminium", desc("Some long option"));

  const char *args[] = {"prog", "--aluminum"};

  std::ostringstream OS;

  EXPECT_FALSE(ParseCommandLineOptions(2, args, "", &OS));
  EXPECT_NE(OS.str().find("prog: Did you mean '--aluminium'?\n"),
            std::string::npos);
  OS.str("");
  OS.clear();

  ResetAllOptionOccurrences();
}

TEST(CommandLineTest, OptionErrorMessageSuggestNoHidden) {
  ResetCommandLineParser();

  StackOption<bool> OptLong("aluminium", desc("Some long option"));
  StackOption<bool> OptLong2("aluminum", desc("Bad option"), ReallyHidden);

  const char *args[] = {"prog", "--alumnum"};

  std::ostringstream OS;

  EXPECT_FALSE(ParseCommandLineOptions(2, args, "", &OS));
  EXPECT_NE(OS.str().find("prog: Did you mean '--aluminium'?\n"),
            std::string::npos);
  OS.str("");
  OS.clear();

  ResetAllOptionOccurrences();
}

TEST(CommandLineTest, Callback) {
  ResetCommandLineParser();

  StackOption<bool> OptA("a", desc("option a"));
  StackOption<bool> OptB("b", desc("option b -- This option turns on option a"),
                         callback([&](const bool &) { OptA = true; }));
  StackOption<bool> OptC(
      "c", desc("option c -- This option turns on options a and b"),
      callback([&](const bool &) { OptB = true; }));
  StackOption<std::string, list<std::string>> List(
      "list",
      desc("option list -- This option turns on options a, b, and c when "
           "'foo' is included in list"),
      CommaSeparated, callback([&](const std::string &Str) {
        if (Str == "foo")
          OptC = true;
      }));

  const char *args1[] = {"prog", "-a"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args1));
  EXPECT_TRUE(OptA);
  EXPECT_FALSE(OptB);
  EXPECT_FALSE(OptC);
  EXPECT_EQ(List.size(), 0u);
  ResetAllOptionOccurrences();

  const char *args2[] = {"prog", "-b"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args2));
  EXPECT_TRUE(OptA);
  EXPECT_TRUE(OptB);
  EXPECT_FALSE(OptC);
  EXPECT_EQ(List.size(), 0u);
  ResetAllOptionOccurrences();

  const char *args3[] = {"prog", "-c"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args3));
  EXPECT_TRUE(OptA);
  EXPECT_TRUE(OptB);
  EXPECT_TRUE(OptC);
  EXPECT_EQ(List.size(), 0u);
  ResetAllOptionOccurrences();

  const char *args4[] = {"prog", "--list=foo,bar"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args4));
  EXPECT_TRUE(OptA);
  EXPECT_TRUE(OptB);
  EXPECT_TRUE(OptC);
  EXPECT_EQ(List.size(), 2u);
  ResetAllOptionOccurrences();

  const char *args5[] = {"prog", "--list=bar"};
  EXPECT_TRUE(ParseCommandLineOptions(2, args5));
  EXPECT_FALSE(OptA);
  EXPECT_FALSE(OptB);
  EXPECT_FALSE(OptC);
  EXPECT_EQ(List.size(), 1u);

  ResetAllOptionOccurrences();
}

enum Enum { Val1, Val2 };
static bits<Enum>
    ExampleBits(desc("An example bits to ensure it compiles"),
                values(clEnumValN(Val1, "bits-val1", "The Val1 value"),
                       clEnumValN(Val1, "bits-val2", "The Val2 value")));

TEST(CommandLineTest, ConsumeAfterOnePositional) {
  ResetCommandLineParser();

  StackOption<std::string, opt<std::string>> Input(Positional, Required);
  StackOption<std::string, list<std::string>> ExtraArgs(ConsumeAfter);

  const char *Args[] = {"prog", "input", "arg1", "arg2"};

  std::ostringstream OS;
  EXPECT_TRUE(ParseCommandLineOptions(4, Args, "", &OS));
  EXPECT_EQ("input", Input);
  EXPECT_EQ(ExtraArgs.size(), 2u);
  EXPECT_EQ(ExtraArgs[0], "arg1");
  EXPECT_EQ(ExtraArgs[1], "arg2");
  EXPECT_TRUE(OS.str().empty());
}

TEST(CommandLineTest, ConsumeAfterTwoPositionals) {
  ResetCommandLineParser();

  StackOption<std::string, opt<std::string>> Input1(Positional, Required);
  StackOption<std::string, opt<std::string>> Input2(Positional, Required);
  StackOption<std::string, list<std::string>> ExtraArgs(ConsumeAfter);

  const char *Args[] = {"prog", "input1", "input2", "arg1", "arg2"};

  std::ostringstream OS;
  EXPECT_TRUE(ParseCommandLineOptions(5, Args, "", &OS));
  EXPECT_EQ("input1", Input1);
  EXPECT_EQ("input2", Input2);
  EXPECT_EQ(ExtraArgs.size(), 2u);
  EXPECT_EQ(ExtraArgs[0], "arg1");
  EXPECT_EQ(ExtraArgs[1], "arg2");
  EXPECT_TRUE(OS.str().empty());
}

TEST(CommandLineTest, ConsumeOptionalString) {
  ResetCommandLineParser();

  StackOption<std::optional<std::string>, opt<std::optional<std::string>>>
      Input("input");

  const char *Args[] = {"prog", "--input=\"value\""};

  std::ostringstream OS;
  ASSERT_TRUE(ParseCommandLineOptions(2, Args, "", &OS));
  ASSERT_TRUE(Input.has_value());
  EXPECT_EQ("\"value\"", *Input);
  EXPECT_TRUE(OS.str().empty());
}

TEST(CommandLineTest, ResetAllOptionOccurrences) {
  ResetCommandLineParser();

  StackOption<bool> Option("option");
  StackOption<std::string> Str("str");
  enum Vals { ValA, ValB, ValC };
  StackOption<Vals, bits<Vals>> Bits(
      values(clEnumValN(ValA, "enableA", "Enable A"),
             clEnumValN(ValB, "enableB", "Enable B"),
             clEnumValN(ValC, "enableC", "Enable C")));
  StackOption<std::string, list<std::string>> Sink(llcl::Sink);
  StackOption<std::string> Input(Positional);
  StackOption<std::string, list<std::string>> ExtraArgs(ConsumeAfter);

  const char *Args[] = {"prog",     "-option",  "-str=STR", "-enableA",
                        "-enableC", "-unknown", "input",    "-arg"};

  std::ostringstream OS;
  EXPECT_TRUE(ParseCommandLineOptions(8, Args, "", &OS));
  EXPECT_TRUE(OS.str().empty());

  EXPECT_TRUE(Option);
  EXPECT_EQ("STR", Str);
  EXPECT_EQ((1u << ValA) | (1u << ValC), Bits.getBits());
  EXPECT_EQ(1u, Sink.size());
  EXPECT_EQ("-unknown", Sink[0]);
  EXPECT_EQ("input", Input);
  EXPECT_EQ(1u, ExtraArgs.size());
  EXPECT_EQ("-arg", ExtraArgs[0]);

  ResetAllOptionOccurrences();
  EXPECT_FALSE(Option);
  EXPECT_EQ("", Str);
  EXPECT_EQ(0u, Bits.getBits());
  EXPECT_EQ(0u, Sink.size());
  EXPECT_EQ(0, Input.getNumOccurrences());
  EXPECT_EQ(0u, ExtraArgs.size());
}

TEST(CommandLineTest, DefaultValue) {
  ResetCommandLineParser();

  StackOption<bool> BoolOption("bool-option");
  StackOption<std::string> StrOption("str-option");
  StackOption<bool> BoolInitOption("bool-init-option", init(true));
  StackOption<std::string> StrInitOption("str-init-option",
                                         init("str-default-value"));

  const char *Args[] = {"prog"};

  std::ostringstream OS;
  EXPECT_TRUE(ParseCommandLineOptions(1, Args, "", &OS));
  EXPECT_TRUE(OS.str().empty());

  EXPECT_TRUE(!BoolOption);
  EXPECT_FALSE(BoolOption.Default.hasValue());
  EXPECT_EQ(0, BoolOption.getNumOccurrences());

  EXPECT_EQ("", StrOption);
  EXPECT_FALSE(StrOption.Default.hasValue());
  EXPECT_EQ(0, StrOption.getNumOccurrences());

  EXPECT_TRUE(BoolInitOption);
  EXPECT_TRUE(BoolInitOption.Default.hasValue());
  EXPECT_EQ(0, BoolInitOption.getNumOccurrences());

  EXPECT_EQ("str-default-value", StrInitOption);
  EXPECT_TRUE(StrInitOption.Default.hasValue());
  EXPECT_EQ(0, StrInitOption.getNumOccurrences());

  const char *Args2[] = {"prog", "-bool-option", "-str-option=str-value",
                         "-bool-init-option=0",
                         "-str-init-option=str-init-value"};

  EXPECT_TRUE(ParseCommandLineOptions(5, Args2, "", &OS));
  EXPECT_TRUE(OS.str().empty());

  EXPECT_TRUE(BoolOption);
  EXPECT_FALSE(BoolOption.Default.hasValue());
  EXPECT_EQ(1, BoolOption.getNumOccurrences());

  EXPECT_EQ("str-value", StrOption);
  EXPECT_FALSE(StrOption.Default.hasValue());
  EXPECT_EQ(1, StrOption.getNumOccurrences());

  EXPECT_FALSE(BoolInitOption);
  EXPECT_TRUE(BoolInitOption.Default.hasValue());
  EXPECT_EQ(1, BoolInitOption.getNumOccurrences());

  EXPECT_EQ("str-init-value", StrInitOption);
  EXPECT_TRUE(StrInitOption.Default.hasValue());
  EXPECT_EQ(1, StrInitOption.getNumOccurrences());
}

//===----------------------------------------------------------------------===//
// PrintOptionInfo / PrintOptionValue tests (require interceptStdout)
//===----------------------------------------------------------------------===//

template <void (*Func)(const Option &)>
class PrintOptionTestBase : public ::testing::Test {
public:
  template <typename... Ts> std::string runTest(Ts... OptionAttributes) {
    StackOption<OptionValue> TestOption(Opt, desc(HelpText),
                                        OptionAttributes...);
    return interceptStdout([&]() { Func(TestOption); });
  }

  enum class OptionValue { Val };
  const std::string Opt = "some-option";
  const std::string HelpText = "some help";
};

void printOptionInfo(const Option &O) { O.printOptionInfo(/*GlobalWidth=*/26); }

using PrintOptionInfoTest = PrintOptionTestBase<printOptionInfo>;

TEST_F(PrintOptionInfoTest, PrintOptionInfoValueOptionalWithoutSentinel) {
  std::string Output = runTest(
      ValueOptional, values(clEnumValN(OptionValue::Val, "v1", "desc1")));

  EXPECT_EQ(Output, ("  --" + Opt + "=<value> - " + HelpText +
                     "\n"
                     "    =v1                 -   desc1\n"));
}

TEST_F(PrintOptionInfoTest, PrintOptionInfoValueOptionalWithSentinel) {
  std::string Output =
      runTest(ValueOptional, values(clEnumValN(OptionValue::Val, "v1", "desc1"),
                                    clEnumValN(OptionValue::Val, "", "")));

  EXPECT_EQ(Output, ("  --" + Opt + "         - " + HelpText +
                     "\n"
                     "  --" +
                     Opt + "=<value> - " + HelpText +
                     "\n"
                     "    =v1                 -   desc1\n"));
}

TEST_F(PrintOptionInfoTest, PrintOptionInfoValueOptionalWithSentinelWithHelp) {
  std::string Output =
      runTest(ValueOptional, values(clEnumValN(OptionValue::Val, "v1", "desc1"),
                                    clEnumValN(OptionValue::Val, "", "desc2")));

  EXPECT_EQ(Output, ("  --" + Opt + "         - " + HelpText +
                     "\n"
                     "  --" +
                     Opt + "=<value> - " + HelpText +
                     "\n"
                     "    =v1                 -   desc1\n"
                     "    =<empty>            -   desc2\n"));
}

TEST_F(PrintOptionInfoTest, PrintOptionInfoValueRequiredWithEmptyValueName) {
  std::string Output =
      runTest(ValueRequired, values(clEnumValN(OptionValue::Val, "v1", "desc1"),
                                    clEnumValN(OptionValue::Val, "", "")));

  EXPECT_EQ(Output, ("  --" + Opt + "=<value> - " + HelpText +
                     "\n"
                     "    =v1                 -   desc1\n"
                     "    =<empty>\n"));
}

TEST_F(PrintOptionInfoTest, PrintOptionInfoEmptyValueDescription) {
  std::string Output =
      runTest(ValueRequired, values(clEnumValN(OptionValue::Val, "v1", "")));

  EXPECT_EQ(Output, ("  --" + Opt + "=<value> - " + HelpText +
                     "\n"
                     "    =v1\n"));
}

TEST_F(PrintOptionInfoTest, PrintOptionInfoMultilineValueDescription) {
  std::string Output = runTest(
      ValueRequired, values(clEnumValN(OptionValue::Val, "v1",
                                       "This is the first enum value\n"
                                       "which has a really long description\n"
                                       "thus it is multi-line."),
                            clEnumValN(OptionValue::Val, "",
                                       "This is an unnamed enum value\n"
                                       "Should be indented as well")));

  EXPECT_EQ(Output,
            ("  --" + Opt + "=<value> - " + HelpText +
             "\n"
             "    =v1                 -   This is the first enum value\n"
             "                            which has a really long description\n"
             "                            thus it is multi-line.\n"
             "    =<empty>            -   This is an unnamed enum value\n"
             "                            Should be indented as well\n"));
}

void printOptionValue(const Option &O) {
  O.printOptionValue(/*GlobalWidth=*/12, /*Force=*/true);
}

using PrintOptionValueTest = PrintOptionTestBase<printOptionValue>;

TEST_F(PrintOptionValueTest, PrintOptionDefaultValue) {
  std::string Output =
      runTest(init(OptionValue::Val),
              values(clEnumValN(OptionValue::Val, "v1", "desc1")));

  EXPECT_EQ(Output, ("    --" + Opt + " = v1       (default: v1)\n"));
}

TEST_F(PrintOptionValueTest, PrintOptionNoDefaultValue) {
  std::string Output =
      runTest(values(clEnumValN(OptionValue::Val, "v1", "desc1")));

  EXPECT_EQ(Output, ("    --" + Opt + " = v1       (default: )\n"));
}

TEST_F(PrintOptionValueTest, PrintOptionUnknownValue) {
  std::string Output = runTest(init(OptionValue::Val));

  EXPECT_EQ(Output, ("    --" + Opt + " = *unknown option value*\n"));
}

class GetOptionWidthTest : public ::testing::Test {
public:
  enum class OptionValue { Val };

  template <typename... Ts>
  size_t runTest(std::string_view ArgName, Ts... OptionAttributes) {
    StackOption<OptionValue> TestOption(ArgName, desc("some help"),
                                        OptionAttributes...);
    return getOptionWidth(TestOption);
  }

private:
  size_t getOptionWidth(const Option &O) { return O.getOptionWidth(); }
};

TEST_F(GetOptionWidthTest, GetOptionWidthArgNameLonger) {
  std::string ArgName("a-long-argument-name");
  size_t ExpectedStrSize = ("  --" + ArgName + "=<value> - ").size();
  EXPECT_EQ(runTest(ArgName, values(clEnumValN(OptionValue::Val, "v", "help"))),
            ExpectedStrSize);
}

TEST_F(GetOptionWidthTest, GetOptionWidthFirstOptionNameLonger) {
  std::string OptName("a-long-option-name");
  size_t ExpectedStrSize = ("    =" + OptName + " - ").size();
  EXPECT_EQ(runTest("a", values(clEnumValN(OptionValue::Val, OptName, "help"),
                                clEnumValN(OptionValue::Val, "b", "help"))),
            ExpectedStrSize);
}

TEST_F(GetOptionWidthTest, GetOptionWidthSecondOptionNameLonger) {
  std::string OptName("a-long-option-name");
  size_t ExpectedStrSize = ("    =" + OptName + " - ").size();
  EXPECT_EQ(runTest("a", values(clEnumValN(OptionValue::Val, "b", "help"),
                                clEnumValN(OptionValue::Val, OptName, "help"))),
            ExpectedStrSize);
}

TEST_F(GetOptionWidthTest, GetOptionWidthEmptyOptionNameLonger) {
  size_t ExpectedStrSize = std::string_view("    =<empty> - ").size();
  EXPECT_EQ(runTest("a", values(clEnumValN(OptionValue::Val, "b", "help"),
                                clEnumValN(OptionValue::Val, "", "help"))),
            ExpectedStrSize);
}

TEST_F(GetOptionWidthTest,
       GetOptionWidthValueOptionalEmptyOptionWithNoDescription) {
  std::string ArgName("a");
  size_t ExpectedStrSize = ("  -" + ArgName + "=<value> - ").size();
  EXPECT_EQ(runTest(ArgName, ValueOptional,
                    values(clEnumValN(OptionValue::Val, "value", "help"),
                           clEnumValN(OptionValue::Val, "", ""))),
            ExpectedStrSize);
}

TEST_F(GetOptionWidthTest,
       GetOptionWidthValueRequiredEmptyOptionWithNoDescription) {
  size_t ExpectedStrSize = std::string_view("    =<empty> - ").size();
  EXPECT_EQ(runTest("a", ValueRequired,
                    values(clEnumValN(OptionValue::Val, "value", "help"),
                           clEnumValN(OptionValue::Val, "", ""))),
            ExpectedStrSize);
}

TEST(CommandLineTest, HelpWithoutSubcommands) {
  ResetCommandLineParser();
  StackOption<bool> Opt("opt", init(false));
  const char *args[] = {"prog"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args), args, "", &nulls()));
  auto Output = interceptStdout([]() { PrintHelpMessage(); });
  EXPECT_NE(std::string::npos, Output.find("USAGE: prog [options]")) << Output;
  EXPECT_EQ(std::string::npos, Output.find("SUBCOMMANDS:")) << Output;
  ResetCommandLineParser();
}

TEST(CommandLineTest, HelpWithSubcommands) {
  ResetCommandLineParser();
  StackSubCommand SC1("sc1", "First Subcommand");
  StackSubCommand SC2("sc2", "Second Subcommand");
  StackOption<bool> SC1Opt("sc1", sub(SC1), init(false));
  StackOption<bool> SC2Opt("sc2", sub(SC2), init(false));
  const char *args[] = {"prog"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args), args, "", &nulls()));
  auto Output = interceptStdout([]() { PrintHelpMessage(); });
  EXPECT_NE(std::string::npos,
            Output.find("USAGE: prog [subcommand] [options]"))
      << Output;
  EXPECT_NE(std::string::npos, Output.find("SUBCOMMANDS:")) << Output;
  EXPECT_NE(std::string::npos, Output.find("sc1 - First Subcommand")) << Output;
  EXPECT_NE(std::string::npos, Output.find("sc2 - Second Subcommand"))
      << Output;
  ResetCommandLineParser();
}

TEST(CommandLineTest, UnknownCommands) {
  ResetCommandLineParser();

  StackSubCommand SC1("foo", "Foo subcommand");
  StackSubCommand SC2("bar", "Bar subcommand");
  StackOption<bool> SC1Opt("put", sub(SC1));
  StackOption<bool> SC2Opt("get", sub(SC2));
  StackOption<bool> TopOpt1("peek");
  StackOption<bool> TopOpt2("set");

  std::ostringstream OS;

  const char *Args1[] = {"prog", "baz", "--get"};
  EXPECT_FALSE(ParseCommandLineOptions(std::size(Args1), Args1, "", &OS));
  EXPECT_EQ(OS.str(),
            "prog: Unknown subcommand 'baz'.  Try: 'prog --help'\n"
            "prog: Did you mean 'bar'?\n"
            "prog: Unknown command line argument '--get'.  Try: 'prog "
            "--help'\n"
            "prog: Did you mean '--set'?\n");

  OS.str("");
  OS.clear();
  const char *Args2[] = {"prog", "faz"};
  EXPECT_FALSE(ParseCommandLineOptions(std::size(Args2), Args2, "", &OS));
  EXPECT_EQ(OS.str(), "prog: Unknown subcommand 'faz'.  Try: 'prog --help'\n");
}

TEST(CommandLineTest, SubCommandGroups) {
  ResetCommandLineParser();

  StackSubCommand SC1("sc1", "SC1 subcommand");
  StackSubCommand SC2("sc2", "SC2 subcommand");
  StackSubCommand SC3("sc3", "SC3 subcommand");
  SubCommandGroup Group12 = {&SC1, &SC2};

  StackOption<bool> Opt12("opt12", sub(Group12), init(false));
  StackOption<bool> Opt3("opt3", sub(SC3), init(false));

  EXPECT_EQ(1U, SC1.OptionsMap.size());
  EXPECT_TRUE(SC1.OptionsMap.count("opt12") > 0);

  EXPECT_EQ(1U, SC2.OptionsMap.size());
  EXPECT_TRUE(SC2.OptionsMap.count("opt12") > 0);

  EXPECT_FALSE(SubCommand::getTopLevel().OptionsMap.count("opt12") > 0);
  EXPECT_FALSE(SC3.OptionsMap.count("opt12") > 0);
}

TEST(CommandLineTest, HelpWithEmptyCategory) {
  ResetCommandLineParser();

  OptionCategory Category1("First Category");
  OptionCategory Category2("Second Category");
  StackOption<int> Opt1("opt1", cat(Category1));
  StackOption<int> Opt2("opt2", cat(Category2));
  HideUnrelatedOptions(Category2);

  const char *args[] = {"prog"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args), args, "", &nulls()));
  auto Output = interceptStdout(
      []() { PrintHelpMessage(/*Hidden=*/false, /*Categorized=*/true); });
  EXPECT_EQ(std::string::npos, Output.find("First Category"))
      << "An empty category should not be printed";

  Output = interceptStdout(
      []() { PrintHelpMessage(/*Hidden=*/true, /*Categorized=*/true); });
  EXPECT_EQ(std::string::npos, Output.find("First Category"))
      << "An empty category should not be printed";

  ResetCommandLineParser();
}

//===----------------------------------------------------------------------===//
// Shell Completion Generation tests
//===----------------------------------------------------------------------===//

TEST(CommandLineTest, BashCompletionBasic) {
  ResetCommandLineParser();

  StackOption<bool> OptVerbose("verbose", desc("Enable verbose output"));
  StackOption<std::string> OptOutput("output", desc("Output file"));

  const char *args[] = {"myprog"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args), args, "", &nulls()));

  auto Output = interceptStdout([]() { PrintShellCompletion("bash"); });

  // Should contain the function name based on program name
  EXPECT_NE(std::string::npos, Output.find("_myprog_completion()")) << Output;
  // Should contain complete command
  EXPECT_NE(std::string::npos,
            Output.find("complete -F _myprog_completion myprog"))
      << Output;
  // Should contain our options
  EXPECT_NE(std::string::npos, Output.find("--verbose")) << Output;
  EXPECT_NE(std::string::npos, Output.find("--output")) << Output;
  // Should contain _init_completion
  EXPECT_NE(std::string::npos, Output.find("_init_completion")) << Output;

  ResetCommandLineParser();
}

TEST(CommandLineTest, BashCompletionWithEnumValues) {
  ResetCommandLineParser();

  enum Format { JSON, XML, YAML };
  StackOption<Format> OptFormat(
      "format", desc("Output format"),
      values(clEnumValN(JSON, "json", "JSON format"),
             clEnumValN(XML, "xml", "XML format"),
             clEnumValN(YAML, "yaml", "YAML format")));

  const char *args[] = {"myprog"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args), args, "", &nulls()));

  auto Output = interceptStdout([]() { PrintShellCompletion("bash"); });

  // Should have value completion for --format
  EXPECT_NE(std::string::npos, Output.find("--format")) << Output;
  EXPECT_NE(std::string::npos, Output.find("json")) << Output;
  EXPECT_NE(std::string::npos, Output.find("xml")) << Output;
  EXPECT_NE(std::string::npos, Output.find("yaml")) << Output;

  ResetCommandLineParser();
}

TEST(CommandLineTest, BashCompletionWithSubcommands) {
  ResetCommandLineParser();

  StackSubCommand SC1("build", "Build the project");
  StackSubCommand SC2("test", "Run tests");
  StackOption<bool> SC1Opt("release", sub(SC1), desc("Release build"));
  StackOption<bool> SC2Opt("verbose", sub(SC2), desc("Verbose test output"));

  const char *args[] = {"myprog"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args), args, "", &nulls()));

  auto Output = interceptStdout([]() { PrintShellCompletion("bash"); });

  // Should contain subcommand names
  EXPECT_NE(std::string::npos, Output.find("build")) << Output;
  EXPECT_NE(std::string::npos, Output.find("test")) << Output;
  // Should have per-subcommand case
  EXPECT_NE(std::string::npos, Output.find("--release")) << Output;
  EXPECT_NE(std::string::npos, Output.find("--verbose")) << Output;

  ResetCommandLineParser();
}

TEST(CommandLineTest, ZshCompletionBasic) {
  ResetCommandLineParser();

  StackOption<bool> OptVerbose("verbose", desc("Enable verbose output"));
  StackOption<std::string> OptOutput("output", desc("Output file"));

  const char *args[] = {"myprog"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args), args, "", &nulls()));

  auto Output = interceptStdout([]() { PrintShellCompletion("zsh"); });

  // Should contain compdef
  EXPECT_NE(std::string::npos, Output.find("#compdef myprog")) << Output;
  // Should contain function
  EXPECT_NE(std::string::npos, Output.find("_myprog()")) << Output;
  // Should contain _arguments
  EXPECT_NE(std::string::npos, Output.find("_arguments")) << Output;
  // Should contain our options with descriptions
  EXPECT_NE(std::string::npos, Output.find("--verbose")) << Output;
  EXPECT_NE(std::string::npos, Output.find("--output")) << Output;

  ResetCommandLineParser();
}

TEST(CommandLineTest, ZshCompletionWithEnumValues) {
  ResetCommandLineParser();

  enum Format { JSON, XML, YAML };
  StackOption<Format> OptFormat(
      "format", desc("Output format"),
      values(clEnumValN(JSON, "json", "JSON format"),
             clEnumValN(XML, "xml", "XML format"),
             clEnumValN(YAML, "yaml", "YAML format")));

  const char *args[] = {"myprog"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args), args, "", &nulls()));

  auto Output = interceptStdout([]() { PrintShellCompletion("zsh"); });

  // Should contain value list for enum option
  EXPECT_NE(std::string::npos, Output.find("--format")) << Output;
  EXPECT_NE(std::string::npos, Output.find("json")) << Output;
  EXPECT_NE(std::string::npos, Output.find("xml")) << Output;
  EXPECT_NE(std::string::npos, Output.find("yaml")) << Output;

  ResetCommandLineParser();
}

TEST(CommandLineTest, ZshCompletionWithSubcommands) {
  ResetCommandLineParser();

  StackSubCommand SC1("build", "Build the project");
  StackSubCommand SC2("test", "Run tests");
  StackOption<bool> SC1Opt("release", sub(SC1), desc("Release build"));
  StackOption<bool> SC2Opt("verbose", sub(SC2), desc("Verbose test output"));

  const char *args[] = {"myprog"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args), args, "", &nulls()));

  auto Output = interceptStdout([]() { PrintShellCompletion("zsh"); });

  // Should contain subcommand descriptions
  EXPECT_NE(std::string::npos, Output.find("build")) << Output;
  EXPECT_NE(std::string::npos, Output.find("test")) << Output;
  // Should contain per-subcommand functions
  EXPECT_NE(std::string::npos, Output.find("_myprog_build()")) << Output;
  EXPECT_NE(std::string::npos, Output.find("_myprog_test()")) << Output;
  // Should contain subcommand-specific options
  EXPECT_NE(std::string::npos, Output.find("--release")) << Output;
  EXPECT_NE(std::string::npos, Output.find("--verbose")) << Output;

  ResetCommandLineParser();
}

TEST(CommandLineTest, CompletionHidesReallyHiddenOptions) {
  ResetCommandLineParser();

  StackOption<bool> OptVisible("visible", desc("Visible option"));
  StackOption<bool> OptHidden("hidden-opt", desc("Hidden option"), Hidden);
  StackOption<bool> OptReallyHidden("really-hidden-opt",
                                    desc("Really hidden option"), ReallyHidden);

  const char *args[] = {"myprog"};
  EXPECT_TRUE(ParseCommandLineOptions(std::size(args), args, "", &nulls()));

  auto Output = interceptStdout([]() { PrintShellCompletion("bash"); });

  // Visible and Hidden should be present
  EXPECT_NE(std::string::npos, Output.find("--visible")) << Output;
  EXPECT_NE(std::string::npos, Output.find("--hidden-opt")) << Output;
  // ReallyHidden should NOT be present
  EXPECT_EQ(std::string::npos, Output.find("--really-hidden-opt")) << Output;

  ResetCommandLineParser();
}

} // anonymous namespace
