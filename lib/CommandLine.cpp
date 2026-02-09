//===-- CommandLine.cpp - Command line parser implementation --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Standalone C++17 extraction of the LLVM CommandLine library.
// All LLVM dependencies replaced with std:: equivalents.
//
//===----------------------------------------------------------------------===//

#include "llcl/CommandLine.h"

#include <algorithm>
#include <cassert>
#include <charconv>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <numeric>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>

using namespace llcl;

//===----------------------------------------------------------------------===//
// Anchors for virtual function tables
//
void GenericOptionValue::anchor() {}
void OptionValue<boolOrDefault>::anchor() {}
void OptionValue<std::string>::anchor() {}
void Option::anchor() {}
void basic_parser_impl::anchor() {}
void parser<bool>::anchor() {}
void parser<boolOrDefault>::anchor() {}
void parser<int>::anchor() {}
void parser<long>::anchor() {}
void parser<long long>::anchor() {}
void parser<unsigned>::anchor() {}
void parser<unsigned long>::anchor() {}
void parser<unsigned long long>::anchor() {}
void parser<double>::anchor() {}
void parser<float>::anchor() {}
void parser<std::string>::anchor() {}
void parser<std::optional<std::string>>::anchor() {}
void parser<char>::anchor() {}

//===----------------------------------------------------------------------===//
// String utility helpers (anonymous namespace)
//

namespace {

// Levenshtein edit distance (ported from llvm/ADT/edit_distance.h)
unsigned editDistance(std::string_view FromArray, std::string_view ToArray,
                      bool AllowReplacements = true,
                      unsigned MaxEditDistance = 0) {
  size_t m = FromArray.size(), n = ToArray.size();

  if (MaxEditDistance) {
    size_t AbsDiff = m > n ? m - n : n - m;
    if (AbsDiff > MaxEditDistance)
      return MaxEditDistance + 1;
  }

  std::vector<unsigned> Row(n + 1);
  for (unsigned i = 1; i <= n; ++i)
    Row[i] = i;

  for (size_t y = 1; y <= m; ++y) {
    Row[0] = static_cast<unsigned>(y);
    unsigned BestThisRow = Row[0];

    unsigned Previous = static_cast<unsigned>(y - 1);
    for (size_t x = 1; x <= n; ++x) {
      unsigned OldRow = Row[x];
      if (AllowReplacements) {
        Row[x] =
            std::min(Previous + (FromArray[y - 1] == ToArray[x - 1] ? 0u : 1u),
                     std::min(Row[x - 1], Row[x]) + 1);
      } else {
        if (FromArray[y - 1] == ToArray[x - 1])
          Row[x] = Previous;
        else
          Row[x] = std::min(Row[x - 1], Row[x]) + 1;
      }
      Previous = OldRow;
      BestThisRow = std::min(BestThisRow, Row[x]);
    }

    if (MaxEditDistance && BestThisRow > MaxEditDistance)
      return MaxEditDistance + 1;
  }

  return Row[n];
}

// Parse integer with auto-base detection (0x, 0b, 0 prefix)
template <typename T> bool getAsInteger(std::string_view Str, T &Result) {
  if (Str.empty())
    return true;

  int Base = 10;
  std::string_view S = Str;
  bool Negative = false;

  if (S.front() == '-') {
    Negative = true;
    S = S.substr(1);
    if (S.empty())
      return true;
  } else if (S.front() == '+') {
    S = S.substr(1);
    if (S.empty())
      return true;
  }

  if (S.size() > 1 && S[0] == '0') {
    if (S[1] == 'x' || S[1] == 'X') {
      Base = 16;
      S = S.substr(2);
    } else if (S[1] == 'b' || S[1] == 'B') {
      Base = 2;
      S = S.substr(2);
    } else {
      Base = 8;
    }
  }

  if (S.empty())
    return true;

  auto [ptr, ec] = std::from_chars(S.data(), S.data() + S.size(), Result, Base);
  if (ec != std::errc() || ptr != S.data() + S.size())
    return true;

  if (Negative) {
    if constexpr (std::is_signed_v<T>) {
      Result = -Result;
    } else {
      return true; // Can't negate unsigned
    }
  }
  return false;
}

bool starts_with(std::string_view S, std::string_view Prefix) {
  return S.size() >= Prefix.size() && S.substr(0, Prefix.size()) == Prefix;
}

bool consume_front(std::string_view &S, std::string_view Prefix) {
  if (!starts_with(S, Prefix))
    return false;
  S = S.substr(Prefix.size());
  return true;
}

std::pair<std::string_view, std::string_view> split(std::string_view S,
                                                    char Sep) {
  auto Pos = S.find(Sep);
  if (Pos == std::string_view::npos)
    return {S, {}};
  return {S.substr(0, Pos), S.substr(Pos + 1)};
}

// Path utility helpers
std::string_view path_filename(std::string_view Path) {
  auto Pos = Path.find_last_of("/\\");
  if (Pos == std::string_view::npos)
    return Path;
  return Path.substr(Pos + 1);
}

std::string_view path_parent_path(std::string_view Path) {
  auto Pos = Path.find_last_of("/\\");
  if (Pos == std::string_view::npos)
    return {};
  return Path.substr(0, Pos);
}

bool path_is_absolute(std::string_view Path) {
  if (Path.empty())
    return false;
#ifdef _WIN32
  // Drive letter or UNC path
  if (Path.size() >= 2 && Path[1] == ':')
    return true;
  if (Path.size() >= 2 && Path[0] == '\\' && Path[1] == '\\')
    return true;
#endif
  return Path[0] == '/';
}

bool path_is_relative(std::string_view Path) { return !path_is_absolute(Path); }

bool path_has_parent_path(std::string_view Path) {
  return !path_parent_path(Path).empty();
}

std::string path_append(std::string_view Base, std::string_view Component) {
  if (Base.empty())
    return std::string(Component);
  if (Component.empty())
    return std::string(Base);
  std::string Result(Base);
  if (Result.back() != '/' && Result.back() != '\\')
    Result += '/';
  Result += Component;
  return Result;
}

std::string path_native(std::string_view Path) {
  std::string Result(Path);
#ifdef _WIN32
  for (auto &C : Result)
    if (C == '/')
      C = '\\';
#endif
  return Result;
}

// I/O helpers
std::string getEnvVar(const char *Name) {
  const char *Val = std::getenv(Name);
  if (Val)
    return std::string(Val);
  return {};
}

bool to_float(std::string_view Str, double &Result) {
  // std::from_chars for double is not available everywhere in C++17
  // Fall back to strtod
  std::string S(Str);
  char *End = nullptr;
  Result = std::strtod(S.c_str(), &End);
  return End == S.c_str() + S.size();
}

} // anonymous namespace

//===----------------------------------------------------------------------===//

static const size_t DefaultPad = 2;

static std::string_view ArgPrefix = "-";
static std::string_view ArgPrefixLong = "--";
static std::string_view ArgHelpPrefix = " - ";

static size_t argPlusPrefixesSize(std::string_view ArgName,
                                  size_t Pad = DefaultPad) {
  size_t Len = ArgName.size();
  if (Len == 1)
    return Len + Pad + ArgPrefix.size() + ArgHelpPrefix.size();
  return Len + Pad + ArgPrefixLong.size() + ArgHelpPrefix.size();
}

static std::string argPrefix(std::string_view ArgName,
                             size_t Pad = DefaultPad) {
  std::string Prefix;
  for (size_t I = 0; I < Pad; ++I)
    Prefix.push_back(' ');
  Prefix.append(ArgName.size() > 1 ? ArgPrefixLong : ArgPrefix);
  return Prefix;
}

// Option predicates...
static inline bool isGrouping(const Option *O) {
  return O->getMiscFlags() & llcl::Grouping;
}
static inline bool isPrefixedOrGrouping(const Option *O) {
  return isGrouping(O) || O->getFormattingFlag() == llcl::Prefix ||
         O->getFormattingFlag() == llcl::AlwaysPrefix;
}

using OptionsMapTy = std::map<std::string, Option *, std::less<>>;

namespace {

class PrintArg {
  std::string_view ArgName;
  size_t Pad;

public:
  PrintArg(std::string_view ArgName, size_t Pad = DefaultPad)
      : ArgName(ArgName), Pad(Pad) {}
  friend std::ostream &operator<<(std::ostream &OS, const PrintArg &);
};

std::ostream &operator<<(std::ostream &OS, const PrintArg &Arg) {
  OS << argPrefix(Arg.ArgName, Arg.Pad) << Arg.ArgName;
  return OS;
}

class CommandLineParser {
public:
  std::string ProgramName;
  std::string_view ProgramOverview;

  std::vector<std::string_view> MoreHelp;

  std::vector<Option *> DefaultOptions;

  std::unordered_set<OptionCategory *> RegisteredOptionCategories;

  std::unordered_set<SubCommand *> RegisteredSubCommands;

  CommandLineParser() { registerSubCommand(&SubCommand::getTopLevel()); }

  void ResetAllOptionOccurrences();

  bool ParseCommandLineOptions(int argc, const char *const *argv,
                               std::string_view Overview,
                               std::ostream *Errs = nullptr,
                               bool LongOptionsUseDoubleDash = false);

  void forEachSubCommand(Option &Opt,
                         std::function<void(SubCommand &)> Action) {
    if (Opt.Subs.empty()) {
      Action(SubCommand::getTopLevel());
      return;
    }
    if (Opt.Subs.size() == 1 && *Opt.Subs.begin() == &SubCommand::getAll()) {
      for (auto *SC : RegisteredSubCommands)
        Action(*SC);
      Action(SubCommand::getAll());
      return;
    }
    for (auto *SC : Opt.Subs) {
      assert(SC != &SubCommand::getAll() &&
             "SubCommand::getAll() should not be used with other subcommands");
      Action(*SC);
    }
  }

  void addLiteralOption(Option &Opt, SubCommand *SC, std::string_view Name) {
    if (Opt.hasArgStr())
      return;
    auto [it, inserted] =
        SC->OptionsMap.insert(std::make_pair(std::string(Name), &Opt));
    if (!inserted) {
      std::cerr << ProgramName << ": CommandLine Error: Option '" << Name
                << "' registered more than once!\n";
      report_fatal_error("inconsistency in registered CommandLine options");
    }
  }

  void addLiteralOption(Option &Opt, std::string_view Name) {
    forEachSubCommand(
        Opt, [&](SubCommand &SC) { addLiteralOption(Opt, &SC, Name); });
  }

  void addOption(Option *O, SubCommand *SC) {
    bool HadErrors = false;
    if (O->hasArgStr()) {
      if (O->isDefaultOption() &&
          SC->OptionsMap.find(std::string(O->ArgStr)) != SC->OptionsMap.end())
        return;

      auto [it, inserted] =
          SC->OptionsMap.insert(std::make_pair(std::string(O->ArgStr), O));
      if (!inserted) {
        std::cerr << ProgramName << ": CommandLine Error: Option '" << O->ArgStr
                  << "' registered more than once!\n";
        HadErrors = true;
      }
    }

    if (O->getFormattingFlag() == llcl::Positional)
      SC->PositionalOpts.push_back(O);
    else if (O->getMiscFlags() & llcl::Sink)
      SC->SinkOpts.push_back(O);
    else if (O->getNumOccurrencesFlag() == llcl::ConsumeAfter) {
      if (SC->ConsumeAfterOpt) {
        O->error(
            "Cannot specify more than one option with llcl::ConsumeAfter!");
        HadErrors = true;
      }
      SC->ConsumeAfterOpt = O;
    }

    if (HadErrors)
      report_fatal_error("inconsistency in registered CommandLine options");
  }

  void addOption(Option *O, bool ProcessDefaultOption = false) {
    if (!ProcessDefaultOption && O->isDefaultOption()) {
      DefaultOptions.push_back(O);
      return;
    }
    forEachSubCommand(*O, [&](SubCommand &SC) { addOption(O, &SC); });
  }

  void removeOption(Option *O, SubCommand *SC) {
    std::vector<std::string_view> OptionNames;
    O->getExtraOptionNames(OptionNames);
    if (O->hasArgStr())
      OptionNames.push_back(O->ArgStr);

    SubCommand &Sub = *SC;
    for (auto Name : OptionNames) {
      auto I = Sub.OptionsMap.find(std::string(Name));
      if (I != Sub.OptionsMap.end() && I->second == O)
        Sub.OptionsMap.erase(I);
    }

    if (O->getFormattingFlag() == llcl::Positional) {
      for (auto It = Sub.PositionalOpts.begin(); It != Sub.PositionalOpts.end();
           ++It) {
        if (*It == O) {
          Sub.PositionalOpts.erase(It);
          break;
        }
      }
    } else if (O->getMiscFlags() & llcl::Sink) {
      for (auto It = Sub.SinkOpts.begin(); It != Sub.SinkOpts.end(); ++It) {
        if (*It == O) {
          Sub.SinkOpts.erase(It);
          break;
        }
      }
    } else if (O == Sub.ConsumeAfterOpt)
      Sub.ConsumeAfterOpt = nullptr;
  }

  void removeOption(Option *O) {
    forEachSubCommand(*O, [&](SubCommand &SC) { removeOption(O, &SC); });
  }

  bool hasOptions(const SubCommand &Sub) const {
    return (!Sub.OptionsMap.empty() || !Sub.PositionalOpts.empty() ||
            nullptr != Sub.ConsumeAfterOpt);
  }

  bool hasOptions() const {
    for (const auto *S : RegisteredSubCommands) {
      if (hasOptions(*S))
        return true;
    }
    return false;
  }

  bool hasNamedSubCommands() const {
    for (const auto *S : RegisteredSubCommands)
      if (!S->getName().empty())
        return true;
    return false;
  }

  SubCommand *getActiveSubCommand() { return ActiveSubCommand; }

  void updateArgStr(Option *O, std::string_view NewName, SubCommand *SC) {
    SubCommand &Sub = *SC;
    auto [it, inserted] =
        Sub.OptionsMap.insert(std::make_pair(std::string(NewName), O));
    if (!inserted) {
      std::cerr << ProgramName << ": CommandLine Error: Option '" << O->ArgStr
                << "' registered more than once!\n";
      report_fatal_error("inconsistency in registered CommandLine options");
    }
    Sub.OptionsMap.erase(std::string(O->ArgStr));
  }

  void updateArgStr(Option *O, std::string_view NewName) {
    forEachSubCommand(*O,
                      [&](SubCommand &SC) { updateArgStr(O, NewName, &SC); });
  }

  void printOptionValues();

  void registerCategory(OptionCategory *cat) {
    RegisteredOptionCategories.insert(cat);
  }

  void registerSubCommand(SubCommand *sub) {
    RegisteredSubCommands.insert(sub);

    assert(sub != &SubCommand::getAll() &&
           "SubCommand::getAll() should not be registered");
    for (auto &E : SubCommand::getAll().OptionsMap) {
      Option *O = E.second;
      if ((O->isPositional() || O->isSink() || O->isConsumeAfter()) ||
          O->hasArgStr())
        addOption(O, sub);
      else
        addLiteralOption(*O, sub, E.first);
    }
  }

  void unregisterSubCommand(SubCommand *sub) {
    RegisteredSubCommands.erase(sub);
  }

  std::unordered_set<SubCommand *> &getRegisteredSubcommands() {
    return RegisteredSubCommands;
  }

  void reset() {
    ActiveSubCommand = nullptr;
    ProgramName.clear();
    ProgramOverview = std::string_view();

    MoreHelp.clear();
    RegisteredOptionCategories.clear();

    ResetAllOptionOccurrences();
    RegisteredSubCommands.clear();

    SubCommand::getTopLevel().reset();
    SubCommand::getAll().reset();
    registerSubCommand(&SubCommand::getTopLevel());

    DefaultOptions.clear();
  }

  SubCommand *ActiveSubCommand = nullptr;

private:
  Option *LookupOption(SubCommand &Sub, std::string_view &Arg,
                       std::string_view &Value);
  Option *LookupLongOption(SubCommand &Sub, std::string_view &Arg,
                           std::string_view &Value,
                           bool LongOptionsUseDoubleDash, bool HaveDoubleDash) {
    Option *Opt = LookupOption(Sub, Arg, Value);
    if (Opt && LongOptionsUseDoubleDash && !HaveDoubleDash && !isGrouping(Opt))
      return nullptr;
    return Opt;
  }
  SubCommand *LookupSubCommand(std::string_view Name,
                               std::string &NearestString);
};

} // namespace

static CommandLineParser &GlobalParser() {
  static CommandLineParser P;
  return P;
}

template <typename T, T TrueVal, T FalseVal>
static bool parseBool(Option &O, [[maybe_unused]] std::string_view ArgName,
                      std::string_view Arg, T &Value) {
  if (Arg == "" || Arg == "true" || Arg == "TRUE" || Arg == "True" ||
      Arg == "1") {
    Value = TrueVal;
    return false;
  }

  if (Arg == "false" || Arg == "FALSE" || Arg == "False" || Arg == "0") {
    Value = FalseVal;
    return false;
  }
  return O.error("'" + std::string(Arg) +
                 "' is invalid value for boolean argument! Try 0 or 1");
}

void llcl::AddLiteralOption(Option &O, std::string_view Name) {
  GlobalParser().addLiteralOption(O, Name);
}

extrahelp::extrahelp(std::string_view Help) : morehelp(Help) {
  GlobalParser().MoreHelp.push_back(Help);
}

void Option::addArgument() {
  GlobalParser().addOption(this);
  FullyInitialized = true;
}

void Option::removeArgument() { GlobalParser().removeOption(this); }

void Option::setArgStr(std::string_view S) {
  if (FullyInitialized)
    GlobalParser().updateArgStr(this, S);
  assert((S.empty() || S[0] != '-') && "Option can't start with '-");
  ArgStr = S;
  if (ArgStr.size() == 1)
    setMiscFlag(Grouping);
}

void Option::addCategory(OptionCategory &C) {
  assert(!Categories.empty() && "Categories cannot be empty.");
  if (&C != &getGeneralCategory() && Categories[0] == &getGeneralCategory())
    Categories[0] = &C;
  else if (std::find(Categories.begin(), Categories.end(), &C) ==
           Categories.end())
    Categories.push_back(&C);
}

void Option::reset() {
  NumOccurrences = 0;
  setDefault();
  if (isDefaultOption())
    removeArgument();
}

void OptionCategory::registerCategory() {
  GlobalParser().registerCategory(this);
}

// Meyer's singletons for SubCommand
SubCommand &SubCommand::getTopLevel() {
  static SubCommand TopLevelSubCommand;
  return TopLevelSubCommand;
}

SubCommand &SubCommand::getAll() {
  static SubCommand AllSubCommands;
  return AllSubCommands;
}

void SubCommand::registerSubCommand() {
  GlobalParser().registerSubCommand(this);
}

void SubCommand::unregisterSubCommand() {
  GlobalParser().unregisterSubCommand(this);
}

void SubCommand::reset() {
  PositionalOpts.clear();
  SinkOpts.clear();
  OptionsMap.clear();
  ConsumeAfterOpt = nullptr;
}

SubCommand::operator bool() const {
  return (GlobalParser().getActiveSubCommand() == this);
}

//===----------------------------------------------------------------------===//
// Basic, shared command line option processing machinery.
//

Option *CommandLineParser::LookupOption(SubCommand &Sub, std::string_view &Arg,
                                        std::string_view &Value) {
  if (Arg.empty())
    return nullptr;
  assert(&Sub != &SubCommand::getAll());

  size_t EqualPos = Arg.find('=');

  if (EqualPos == std::string_view::npos) {
    auto I = Sub.OptionsMap.find(Arg);
    if (I != Sub.OptionsMap.end())
      return I->second;
    return nullptr;
  }

  auto I = Sub.OptionsMap.find(Arg.substr(0, EqualPos));
  if (I == Sub.OptionsMap.end())
    return nullptr;

  auto *O = I->second;
  if (O->getFormattingFlag() == llcl::AlwaysPrefix)
    return nullptr;

  Value = Arg.substr(EqualPos + 1);
  Arg = Arg.substr(0, EqualPos);
  return I->second;
}

SubCommand *CommandLineParser::LookupSubCommand(std::string_view Name,
                                                std::string &NearestString) {
  if (Name.empty())
    return &SubCommand::getTopLevel();
  SubCommand *NearestMatch = nullptr;
  for (auto *S : RegisteredSubCommands) {
    assert(S != &SubCommand::getAll() &&
           "SubCommand::getAll() is not expected in RegisteredSubCommands");
    if (S->getName().empty())
      continue;

    if (S->getName() == Name)
      return S;

    if (!NearestMatch && editDistance(S->getName(), Name) < 2)
      NearestMatch = S;
  }

  if (NearestMatch)
    NearestString = std::string(NearestMatch->getName());

  return &SubCommand::getTopLevel();
}

/// LookupNearestOption
static Option *LookupNearestOption(std::string_view Arg,
                                   const OptionsMapTy &OptionsMap,
                                   std::string &NearestString) {
  if (Arg.empty())
    return nullptr;

  auto [LHS, RHS] = split(Arg, '=');

  Option *Best = nullptr;
  unsigned BestDistance = 0;
  for (const auto &[Key, O] : OptionsMap) {
    if (O->getOptionHiddenFlag() == ReallyHidden)
      continue;

    std::vector<std::string_view> OptionNames;
    O->getExtraOptionNames(OptionNames);
    if (O->hasArgStr())
      OptionNames.push_back(O->ArgStr);

    bool PermitValue = O->getValueExpectedFlag() != llcl::ValueDisallowed;
    std::string_view Flag = PermitValue ? LHS : Arg;
    for (const auto &Name : OptionNames) {
      unsigned Distance = editDistance(Name, Flag,
                                       /*AllowReplacements=*/true,
                                       /*MaxEditDistance=*/BestDistance);
      if (!Best || Distance < BestDistance) {
        Best = O;
        BestDistance = Distance;
        if (RHS.empty() || !PermitValue)
          NearestString = std::string(Name);
        else
          NearestString = std::string(Name) + "=" + std::string(RHS);
      }
    }
  }

  return Best;
}

/// CommaSeparateAndAddOccurrence
static bool CommaSeparateAndAddOccurrence(Option *Handler, unsigned pos,
                                          std::string_view ArgName,
                                          std::string_view Value,
                                          bool MultiArg = false) {
  if (Handler->getMiscFlags() & CommaSeparated) {
    std::string_view Val(Value);
    auto Pos = Val.find(',');

    while (Pos != std::string_view::npos) {
      if (Handler->addOccurrence(pos, ArgName, Val.substr(0, Pos), MultiArg))
        return true;
      Val = Val.substr(Pos + 1);
      Pos = Val.find(',');
    }

    Value = Val;
  }

  return Handler->addOccurrence(pos, ArgName, Value, MultiArg);
}

/// ProvideOption
static inline bool ProvideOption(Option *Handler, std::string_view ArgName,
                                 std::string_view Value, int argc,
                                 const char *const *argv, int &i) {
  unsigned NumAdditionalVals = Handler->getNumAdditionalVals();

  switch (Handler->getValueExpectedFlag()) {
  case ValueRequired:
    if (Value.data() == nullptr) { // No value specified?
      if (i + 1 >= argc || Handler->getFormattingFlag() == llcl::AlwaysPrefix)
        return Handler->error("requires a value!");
      assert(argv && "null check");
      Value = std::string_view(argv[++i]);
    }
    break;
  case ValueDisallowed:
    if (NumAdditionalVals > 0)
      return Handler->error("multi-valued option specified"
                            " with ValueDisallowed modifier!");

    if (Value.data() != nullptr)
      return Handler->error("does not allow a value! '" + std::string(Value) +
                            "' specified.");
    break;
  case ValueOptional:
    break;
  }

  if (NumAdditionalVals == 0)
    return CommaSeparateAndAddOccurrence(Handler, i, ArgName, Value);

  bool MultiArg = false;

  if (Value.data() != nullptr) {
    if (CommaSeparateAndAddOccurrence(Handler, i, ArgName, Value, MultiArg))
      return true;
    --NumAdditionalVals;
    MultiArg = true;
  }

  while (NumAdditionalVals > 0) {
    if (i + 1 >= argc)
      return Handler->error("not enough values!");
    assert(argv && "null check");
    Value = std::string_view(argv[++i]);

    if (CommaSeparateAndAddOccurrence(Handler, i, ArgName, Value, MultiArg))
      return true;
    MultiArg = true;
    --NumAdditionalVals;
  }
  return false;
}

bool llcl::ProvidePositionalOption(Option *Handler, std::string_view Arg,
                                   int i) {
  int Dummy = i;
  return ProvideOption(Handler, Handler->ArgStr, Arg, 0, nullptr, Dummy);
}

// getOptionPred
static Option *getOptionPred(std::string_view Name, size_t &Length,
                             bool (*Pred)(const Option *),
                             const OptionsMapTy &OptionsMap) {
  auto OMI = OptionsMap.find(Name);
  if (OMI != OptionsMap.end() && !Pred(OMI->second))
    OMI = OptionsMap.end();

  while (OMI == OptionsMap.end() && Name.size() > 1) {
    Name = Name.substr(0, Name.size() - 1);
    OMI = OptionsMap.find(Name);
    if (OMI != OptionsMap.end() && !Pred(OMI->second))
      OMI = OptionsMap.end();
  }

  if (OMI != OptionsMap.end() && Pred(OMI->second)) {
    Length = Name.size();
    return OMI->second;
  }
  return nullptr;
}

/// HandlePrefixedOrGroupedOption
static Option *HandlePrefixedOrGroupedOption(std::string_view &Arg,
                                             std::string_view &Value,
                                             bool &ErrorParsing,
                                             const OptionsMapTy &OptionsMap) {
  if (Arg.size() == 1)
    return nullptr;

  size_t Length = 0;
  Option *PGOpt = getOptionPred(Arg, Length, isPrefixedOrGrouping, OptionsMap);
  if (!PGOpt)
    return nullptr;

  do {
    std::string_view MaybeValue =
        (Length < Arg.size()) ? Arg.substr(Length) : std::string_view{};
    Arg = Arg.substr(0, Length);

    if (MaybeValue.empty() ||
        PGOpt->getFormattingFlag() == llcl::AlwaysPrefix ||
        (PGOpt->getFormattingFlag() == llcl::Prefix && MaybeValue[0] != '=')) {
      Value = MaybeValue;
      return PGOpt;
    }

    if (MaybeValue[0] == '=') {
      Value = MaybeValue.substr(1);
      return PGOpt;
    }

    assert(isGrouping(PGOpt) && "Broken getOptionPred!");

    if (PGOpt->getValueExpectedFlag() == llcl::ValueRequired) {
      ErrorParsing |= PGOpt->error("may not occur within a group!");
      return nullptr;
    }

    int Dummy = 0;
    ErrorParsing |= ProvideOption(PGOpt, Arg, std::string_view{nullptr, 0}, 0,
                                  nullptr, Dummy);

    Arg = MaybeValue;
    PGOpt = getOptionPred(Arg, Length, isGrouping, OptionsMap);
  } while (PGOpt);

  return nullptr;
}

static bool RequiresValue(const Option *O) {
  return O->getNumOccurrencesFlag() == llcl::Required ||
         O->getNumOccurrencesFlag() == llcl::OneOrMore;
}

static bool EatsUnboundedNumberOfValues(const Option *O) {
  return O->getNumOccurrencesFlag() == llcl::ZeroOrMore ||
         O->getNumOccurrencesFlag() == llcl::OneOrMore;
}

static bool isWhitespace(char C) {
  return C == ' ' || C == '\t' || C == '\r' || C == '\n';
}

static bool isWhitespaceOrNull(char C) { return isWhitespace(C) || C == '\0'; }

static bool isQuote(char C) { return C == '\"' || C == '\''; }

void llcl::TokenizeGNUCommandLine(std::string_view Src, StringSaver &Saver,
                                  std::vector<const char *> &NewArgv,
                                  bool MarkEOLs) {
  std::string Token;
  for (size_t I = 0, E = Src.size(); I != E; ++I) {
    if (Token.empty()) {
      while (I != E && isWhitespace(Src[I])) {
        if (MarkEOLs && Src[I] == '\n')
          NewArgv.push_back(nullptr);
        ++I;
      }
      if (I == E)
        break;
    }

    char C = Src[I];

    if (I + 1 < E && C == '\\') {
      ++I;
      Token.push_back(Src[I]);
      continue;
    }

    if (isQuote(C)) {
      ++I;
      while (I != E && Src[I] != C) {
        if (Src[I] == '\\' && I + 1 != E)
          ++I;
        Token.push_back(Src[I]);
        ++I;
      }
      if (I == E)
        break;
      continue;
    }

    if (isWhitespace(C)) {
      if (!Token.empty())
        NewArgv.push_back(Saver.save(Token).data());
      if (MarkEOLs && C == '\n')
        NewArgv.push_back(nullptr);
      Token.clear();
      continue;
    }

    Token.push_back(C);
  }

  if (!Token.empty())
    NewArgv.push_back(Saver.save(Token).data());
}

/// parseBackslash for Windows tokenization
static size_t parseBackslash(std::string_view Src, size_t I,
                             std::string &Token) {
  size_t E = Src.size();
  int BackslashCount = 0;
  do {
    ++I;
    ++BackslashCount;
  } while (I != E && Src[I] == '\\');

  bool FollowedByDoubleQuote = (I != E && Src[I] == '"');
  if (FollowedByDoubleQuote) {
    Token.append(BackslashCount / 2, '\\');
    if (BackslashCount % 2 == 0)
      return I - 1;
    Token.push_back('"');
    return I;
  }
  Token.append(BackslashCount, '\\');
  return I - 1;
}

static bool isWindowsSpecialChar(char C) {
  return isWhitespaceOrNull(C) || C == '\\' || C == '\"';
}
static bool isWindowsSpecialCharInCommandName(char C) {
  return isWhitespaceOrNull(C) || C == '\"';
}

static inline void
tokenizeWindowsCommandLineImpl(std::string_view Src, StringSaver &Saver,
                               std::function<void(std::string_view)> AddToken,
                               bool AlwaysCopy, std::function<void()> MarkEOL,
                               bool InitialCommandName) {
  std::string Token;

  bool CommandName = InitialCommandName;

  enum { INIT, UNQUOTED, QUOTED } State = INIT;

  for (size_t I = 0, E = Src.size(); I < E; ++I) {
    switch (State) {
    case INIT: {
      assert(Token.empty() && "token should be empty in initial state");
      while (I < E && isWhitespaceOrNull(Src[I])) {
        if (Src[I] == '\n')
          MarkEOL();
        ++I;
      }
      if (I >= E)
        break;
      size_t Start = I;
      if (CommandName) {
        while (I < E && !isWindowsSpecialCharInCommandName(Src[I]))
          ++I;
      } else {
        while (I < E && !isWindowsSpecialChar(Src[I]))
          ++I;
      }
      std::string_view NormalChars = Src.substr(Start, I - Start);
      if (I >= E || isWhitespaceOrNull(Src[I])) {
        AddToken(AlwaysCopy ? Saver.save(NormalChars) : NormalChars);
        if (I < E && Src[I] == '\n') {
          MarkEOL();
          CommandName = InitialCommandName;
        } else {
          CommandName = false;
        }
      } else if (Src[I] == '\"') {
        Token += NormalChars;
        State = QUOTED;
      } else if (Src[I] == '\\') {
        assert(!CommandName && "or else we'd have treated it as a normal char");
        Token += NormalChars;
        I = parseBackslash(Src, I, Token);
        State = UNQUOTED;
      } else {
        assert(false && "unexpected special character");
      }
      break;
    }

    case UNQUOTED:
      if (isWhitespaceOrNull(Src[I])) {
        AddToken(Saver.save(Token));
        Token.clear();
        if (Src[I] == '\n') {
          CommandName = InitialCommandName;
          MarkEOL();
        } else {
          CommandName = false;
        }
        State = INIT;
      } else if (Src[I] == '\"') {
        State = QUOTED;
      } else if (Src[I] == '\\' && !CommandName) {
        I = parseBackslash(Src, I, Token);
      } else {
        Token.push_back(Src[I]);
      }
      break;

    case QUOTED:
      if (Src[I] == '\"') {
        if (I < (E - 1) && Src[I + 1] == '"') {
          Token.push_back('"');
          ++I;
        } else {
          State = UNQUOTED;
        }
      } else if (Src[I] == '\\' && !CommandName) {
        I = parseBackslash(Src, I, Token);
      } else {
        Token.push_back(Src[I]);
      }
      break;
    }
  }

  if (State != INIT)
    AddToken(Saver.save(Token));
}

void llcl::TokenizeWindowsCommandLine(std::string_view Src, StringSaver &Saver,
                                      std::vector<const char *> &NewArgv,
                                      bool MarkEOLs) {
  auto AddToken = [&](std::string_view Tok) { NewArgv.push_back(Tok.data()); };
  auto OnEOL = [&]() {
    if (MarkEOLs)
      NewArgv.push_back(nullptr);
  };
  tokenizeWindowsCommandLineImpl(Src, Saver, AddToken,
                                 /*AlwaysCopy=*/true, OnEOL, false);
}

void llcl::TokenizeWindowsCommandLineNoCopy(
    std::string_view Src, StringSaver &Saver,
    std::vector<std::string_view> &NewArgv) {
  auto AddToken = [&](std::string_view Tok) { NewArgv.push_back(Tok); };
  auto OnEOL = []() {};
  tokenizeWindowsCommandLineImpl(Src, Saver, AddToken, /*AlwaysCopy=*/false,
                                 OnEOL, false);
}

void llcl::TokenizeWindowsCommandLineFull(std::string_view Src,
                                          StringSaver &Saver,
                                          std::vector<const char *> &NewArgv,
                                          bool MarkEOLs) {
  auto AddToken = [&](std::string_view Tok) { NewArgv.push_back(Tok.data()); };
  auto OnEOL = [&]() {
    if (MarkEOLs)
      NewArgv.push_back(nullptr);
  };
  tokenizeWindowsCommandLineImpl(Src, Saver, AddToken,
                                 /*AlwaysCopy=*/true, OnEOL, true);
}

void llcl::tokenizeConfigFile(std::string_view Source, StringSaver &Saver,
                              std::vector<const char *> &NewArgv,
                              bool MarkEOLs) {
  const char *Cur = Source.data();
  const char *End = Source.data() + Source.size();
  for (; Cur != End;) {
    if (isWhitespace(*Cur)) {
      while (Cur != End && isWhitespace(*Cur))
        ++Cur;
      continue;
    }
    if (*Cur == '#') {
      while (Cur != End && *Cur != '\n')
        ++Cur;
      continue;
    }
    std::string Line;
    const char *Start = Cur;
    for (; Cur != End; ++Cur) {
      if (*Cur == '\\') {
        if (Cur + 1 != End) {
          ++Cur;
          if (*Cur == '\n' ||
              (*Cur == '\r' && (Cur + 1 != End) && Cur[1] == '\n')) {
            Line.append(Start, Cur - 1);
            if (*Cur == '\r')
              ++Cur;
            Start = Cur + 1;
          }
        }
      } else if (*Cur == '\n')
        break;
    }
    Line.append(Start, Cur);
    llcl::TokenizeGNUCommandLine(Line, Saver, NewArgv, MarkEOLs);
  }
}

// Check for UTF-8 BOM
static bool hasUTF8ByteOrderMark(std::string_view S) {
  return (S.size() >= 3 && S[0] == '\xef' && S[1] == '\xbb' && S[2] == '\xbf');
}

// Substitute <CFGDIR> with the file's base path.
static void ExpandBasePaths(std::string_view BasePath, StringSaver &Saver,
                            const char *&Arg) {
  assert(path_is_absolute(BasePath));
  const std::string_view Token("<CFGDIR>");
  const std::string_view ArgString(Arg);

  // Simple text substitution: replace all occurrences of <CFGDIR> with BasePath
  std::string Result;
  size_t StartPos = 0;
  for (size_t TokenPos = ArgString.find(Token);
       TokenPos != std::string_view::npos;
       TokenPos = ArgString.find(Token, StartPos)) {
    Result.append(ArgString.substr(StartPos, TokenPos - StartPos));
    Result.append(BasePath);
    StartPos = TokenPos + Token.size();
  }

  if (!Result.empty()) {
    Result.append(ArgString.substr(StartPos));
    Arg = Saver.save(Result).data();
  }
}

// FName must be an absolute path.
Error ExpansionContext::expandResponseFile(std::string_view FName,
                                           std::vector<const char *> &NewArgv) {
  assert(path_is_absolute(FName));
  std::string FNameStr(FName);
  std::ifstream File(FNameStr, std::ios::binary | std::ios::ate);
  if (!File.is_open()) {
    return llcl::Error("cannot not open file '" + FNameStr +
                       "': " + std::string(std::strerror(errno)));
  }

  auto Size = File.tellg();
  File.seekg(0, std::ios::beg);
  std::string Contents(static_cast<size_t>(Size), '\0');
  File.read(Contents.data(), Size);
  File.close();

  std::string_view Str(Contents);

  // If we see UTF-8 BOM sequence at the beginning of a file, skip it.
  if (hasUTF8ByteOrderMark(Str))
    Str = Str.substr(3);

  Tokenizer(Str, Saver, NewArgv, MarkEOLs);

  if (!RelativeNames && !InConfigFile)
    return Error::success();

  std::string_view BasePath = path_parent_path(FName);
  for (const char *&Arg : NewArgv) {
    if (!Arg)
      continue;

    if (InConfigFile)
      ExpandBasePaths(BasePath, Saver, Arg);

    std::string_view ArgStr(Arg);
    std::string_view FileName;
    bool ConfigInclusion = false;
    if (starts_with(ArgStr, "@")) {
      FileName = ArgStr.substr(1);
      if (!path_is_relative(FileName))
        continue;
    } else if (starts_with(ArgStr, "--config=")) {
      FileName = ArgStr.substr(strlen("--config="));
      ConfigInclusion = true;
    } else {
      continue;
    }

    std::string ResponseFile = "@";
    if (ConfigInclusion && !path_has_parent_path(FileName)) {
      std::string FilePath;
      if (!findConfigFile(FileName, FilePath))
        return llcl::Error("cannot not find configuration file: " +
                           std::string(FileName));
      ResponseFile.append(FilePath);
    } else {
      ResponseFile.append(path_append(BasePath, FileName));
    }
    Arg = Saver.save(ResponseFile).data();
  }
  return Error::success();
}

/// Expand response files on a command line recursively.
Error ExpansionContext::expandResponseFiles(std::vector<const char *> &Argv) {
  struct ResponseFileRecord {
    std::string File;
    size_t End;
  };

  std::vector<ResponseFileRecord> FileStack;

  FileStack.push_back({"", Argv.size()});

  for (unsigned I = 0; I != Argv.size();) {
    while (I == FileStack.back().End) {
      FileStack.pop_back();
    }

    const char *Arg = Argv[I];
    if (Arg == nullptr) {
      ++I;
      continue;
    }

    if (Arg[0] != '@') {
      ++I;
      continue;
    }

    const char *FName = Arg + 1;
    std::string CurrDir;
    if (path_is_relative(FName)) {
      if (CurrentDir.empty()) {
        auto CWD = std::filesystem::current_path();
        CurrDir = CWD.string();
      } else {
        CurrDir = std::string(CurrentDir);
      }
      CurrDir = path_append(CurrDir, FName);
      FName = CurrDir.c_str();
    }

    // Check if file exists
    std::error_code FSErr;
    bool PathExists = std::filesystem::exists(FName, FSErr);
    if (!PathExists || FSErr) {
      if (!InConfigFile) {
        ++I;
        continue;
      }
      return llcl::Error("cannot not open file '" + std::string(FName) + "'");
    }
    // If path exists but is not a regular file (e.g., directory), report error
    if (!std::filesystem::is_regular_file(FName)) {
      return llcl::Error("cannot not open file '" + std::string(FName) +
                         "': not a regular file");
    }

    // Check for recursive response files using canonical path
    std::string CanonicalFName;
    {
      std::error_code EC;
      auto P = std::filesystem::canonical(FName, EC);
      if (EC) {
        CanonicalFName = FName;
      } else {
        CanonicalFName = P.string();
      }
    }

    bool IsRecursive = false;
    for (size_t J = 1; J < FileStack.size(); ++J) {
      std::error_code EC2;
      auto P2 = std::filesystem::canonical(FileStack[J].File, EC2);
      if (!EC2 && P2.string() == CanonicalFName) {
        IsRecursive = true;
        break;
      }
    }
    if (IsRecursive) {
      return llcl::Error("recursive expansion of: '" + std::string(FName) +
                         "'");
    }

    std::vector<const char *> ExpandedArgv;
    if (Error Err = expandResponseFile(FName, ExpandedArgv))
      return Err;

    for (ResponseFileRecord &Record : FileStack) {
      Record.End += ExpandedArgv.size() - 1;
    }

    FileStack.push_back({FName, I + ExpandedArgv.size()});
    Argv.erase(Argv.begin() + I);
    Argv.insert(Argv.begin() + I, ExpandedArgv.begin(), ExpandedArgv.end());
  }

  assert(FileStack.size() > 0 && Argv.size() == FileStack.back().End);
  return Error::success();
}

bool llcl::expandResponseFiles(int Argc, const char *const *Argv,
                               const char *EnvVar, StringSaver &Saver,
                               std::vector<const char *> &NewArgv) {
#ifdef _WIN32
  auto Tokenize = llcl::TokenizeWindowsCommandLine;
#else
  auto Tokenize = llcl::TokenizeGNUCommandLine;
#endif
  if (EnvVar) {
    std::string EnvValue = getEnvVar(EnvVar);
    if (!EnvValue.empty())
      Tokenize(EnvValue, Saver, NewArgv, /*MarkEOLs=*/false);
  }

  NewArgv.insert(NewArgv.end(), Argv + 1, Argv + Argc);
  BumpPtrAllocator A;
  ExpansionContext ECtx(A, Tokenize);
  if (Error Err = ECtx.expandResponseFiles(NewArgv)) {
    std::cerr << toString(std::move(Err)) << '\n';
    return false;
  }
  return true;
}

bool llcl::ExpandResponseFiles([[maybe_unused]] StringSaver &Saver,
                               TokenizerCallback Tokenizer,
                               std::vector<const char *> &Argv) {
  BumpPtrAllocator A;
  ExpansionContext ECtx(A, Tokenizer);
  if (Error Err = ECtx.expandResponseFiles(Argv)) {
    std::cerr << toString(std::move(Err)) << '\n';
    return false;
  }
  return true;
}

ExpansionContext::ExpansionContext(BumpPtrAllocator &A, TokenizerCallback T)
    : Saver(A), Tokenizer(T) {}

bool ExpansionContext::findConfigFile(std::string_view FileName,
                                      std::string &FilePath) {
  auto FileExists = [](const std::string &Path) -> bool {
    std::error_code EC;
    auto Status = std::filesystem::status(Path, EC);
    return !EC && std::filesystem::is_regular_file(Status);
  };

  if (path_has_parent_path(FileName)) {
    std::string CfgFilePath(FileName);
    if (path_is_relative(FileName)) {
      auto CWD = std::filesystem::current_path();
      CfgFilePath = path_append(CWD.string(), FileName);
    }
    if (!FileExists(CfgFilePath))
      return false;
    FilePath = CfgFilePath;
    return true;
  }

  for (const auto &Dir : SearchDirs) {
    if (Dir.empty())
      continue;
    std::string CfgFilePath = path_append(Dir, FileName);
    CfgFilePath = path_native(CfgFilePath);
    if (FileExists(CfgFilePath)) {
      FilePath = CfgFilePath;
      return true;
    }
  }

  return false;
}

Error ExpansionContext::readConfigFile(std::string_view CfgFile,
                                       std::vector<const char *> &Argv) {
  std::string AbsPath;
  if (path_is_relative(CfgFile)) {
    auto CWD = std::filesystem::current_path();
    AbsPath = path_append(CWD.string(), CfgFile);
    CfgFile = AbsPath;
  }
  InConfigFile = true;
  RelativeNames = true;
  if (Error Err = expandResponseFile(CfgFile, Argv))
    return Err;
  return expandResponseFiles(Argv);
}

static void initCommonOptions();
bool llcl::ParseCommandLineOptions(int argc, const char *const *argv,
                                   std::string_view Overview,
                                   std::ostream *Errs, const char *EnvVar,
                                   bool LongOptionsUseDoubleDash) {
  initCommonOptions();
  std::vector<const char *> NewArgv;
  BumpPtrAllocator A;
  StringSaver Saver(A);
  NewArgv.push_back(argv[0]);

  if (EnvVar) {
    std::string EnvValue = getEnvVar(EnvVar);
    if (!EnvValue.empty())
      TokenizeGNUCommandLine(EnvValue, Saver, NewArgv);
  }

  for (int I = 1; I < argc; ++I)
    NewArgv.push_back(argv[I]);
  int NewArgc = static_cast<int>(NewArgv.size());

  return GlobalParser().ParseCommandLineOptions(NewArgc, &NewArgv[0], Overview,
                                                Errs, LongOptionsUseDoubleDash);
}

/// Reset all options at least once.
void CommandLineParser::ResetAllOptionOccurrences() {
  // Collect all unique options first, because Option::reset() may call
  // removeArgument() for default options, which modifies OptionsMap.
  std::unordered_set<Option *> AllOptions;
  for (auto *SC : RegisteredSubCommands) {
    for (auto &O : SC->OptionsMap)
      AllOptions.insert(O.second);
    for (Option *O : SC->PositionalOpts)
      AllOptions.insert(O);
    for (Option *O : SC->SinkOpts)
      AllOptions.insert(O);
    if (SC->ConsumeAfterOpt)
      AllOptions.insert(SC->ConsumeAfterOpt);
  }
  for (Option *O : AllOptions)
    O->reset();
}

bool CommandLineParser::ParseCommandLineOptions(int argc,
                                                const char *const *argv,
                                                std::string_view Overview,
                                                std::ostream *Errs,
                                                bool LongOptionsUseDoubleDash) {
  assert(hasOptions() && "No options specified!");

  ProgramOverview = Overview;
  bool IgnoreErrors = Errs;
  if (!Errs)
    Errs = &std::cerr;
  bool ErrorParsing = false;

  // Expand response files.
  std::vector<const char *> newArgv(argv, argv + argc);
  BumpPtrAllocator A;
#ifdef _WIN32
  auto Tokenize = llcl::TokenizeWindowsCommandLine;
#else
  auto Tokenize = llcl::TokenizeGNUCommandLine;
#endif
  ExpansionContext ECtx(A, Tokenize);
  if (Error Err = ECtx.expandResponseFiles(newArgv)) {
    *Errs << toString(std::move(Err)) << '\n';
    return false;
  }
  argv = &newArgv[0];
  argc = static_cast<int>(newArgv.size());

  ProgramName = std::string(path_filename(std::string_view(argv[0])));

  unsigned NumPositionalRequired = 0;

  bool HasUnlimitedPositionals = false;

  int FirstArg = 1;
  SubCommand *ChosenSubCommand = &SubCommand::getTopLevel();
  std::string NearestSubCommandString;
  bool MaybeNamedSubCommand =
      argc >= 2 && argv[FirstArg][0] != '-' && hasNamedSubCommands();
  if (MaybeNamedSubCommand) {
    ChosenSubCommand = LookupSubCommand(std::string_view(argv[FirstArg]),
                                        NearestSubCommandString);
    if (ChosenSubCommand != &SubCommand::getTopLevel())
      FirstArg = 2;
  }
  GlobalParser().ActiveSubCommand = ChosenSubCommand;

  assert(ChosenSubCommand);
  auto &ConsumeAfterOpt = ChosenSubCommand->ConsumeAfterOpt;
  auto &PositionalOpts = ChosenSubCommand->PositionalOpts;
  auto &SinkOpts = ChosenSubCommand->SinkOpts;
  auto &OptionsMap = ChosenSubCommand->OptionsMap;

  for (auto *O : DefaultOptions) {
    addOption(O, true);
  }

  if (ConsumeAfterOpt) {
    assert(PositionalOpts.size() > 0 &&
           "Cannot specify llcl::ConsumeAfter without a positional argument!");
  }
  if (!PositionalOpts.empty()) {
    bool UnboundedFound = false;
    for (size_t i = 0, e = PositionalOpts.size(); i != e; ++i) {
      Option *Opt = PositionalOpts[i];
      if (RequiresValue(Opt))
        ++NumPositionalRequired;
      else if (ConsumeAfterOpt) {
        if (PositionalOpts.size() > 1) {
          if (!IgnoreErrors)
            Opt->error("error - this positional option will never be matched, "
                       "because it does not Require a value, and a "
                       "llcl::ConsumeAfter option is active!");
          ErrorParsing = true;
        }
      } else if (UnboundedFound && !Opt->hasArgStr()) {
        if (!IgnoreErrors)
          Opt->error("error - option can never match, because "
                     "another positional argument will match an "
                     "unbounded number of values, and this option"
                     " does not require a value!");
        *Errs << ProgramName << ": CommandLine Error: Option '" << Opt->ArgStr
              << "' is all messed up!\n";
        *Errs << PositionalOpts.size();
        ErrorParsing = true;
      }
      UnboundedFound |= EatsUnboundedNumberOfValues(Opt);
    }
    HasUnlimitedPositionals = UnboundedFound || ConsumeAfterOpt;
  }

  std::vector<std::pair<std::string_view, unsigned>> PositionalVals;

  Option *ActivePositionalArg = nullptr;

  bool DashDashFound = false;
  for (int i = FirstArg; i < argc; ++i) {
    Option *Handler = nullptr;
    std::string NearestHandlerString;
    std::string_view Value;
    std::string_view ArgName = "";
    bool HaveDoubleDash = false;

    if (argv[i][0] != '-' || argv[i][1] == 0 || DashDashFound) {
      if (ActivePositionalArg) {
        ProvidePositionalOption(ActivePositionalArg, std::string_view(argv[i]),
                                i);
        continue;
      }

      if (!PositionalOpts.empty()) {
        PositionalVals.push_back(
            std::make_pair(std::string_view(argv[i]), (unsigned)i));

        if (PositionalVals.size() >= NumPositionalRequired && ConsumeAfterOpt) {
          for (++i; i < argc; ++i)
            PositionalVals.push_back(
                std::make_pair(std::string_view(argv[i]), (unsigned)i));
          break;
        }

        continue;
      }
    } else if (argv[i][0] == '-' && argv[i][1] == '-' && argv[i][2] == 0 &&
               !DashDashFound) {
      DashDashFound = true;
      continue;
    } else if (ActivePositionalArg &&
               (ActivePositionalArg->getMiscFlags() & PositionalEatsArgs)) {
      ArgName = std::string_view(argv[i] + 1);
      if (consume_front(ArgName, "-"))
        HaveDoubleDash = true;

      Handler = LookupLongOption(*ChosenSubCommand, ArgName, Value,
                                 LongOptionsUseDoubleDash, HaveDoubleDash);
      if (!Handler || Handler->getFormattingFlag() != llcl::Positional) {
        ProvidePositionalOption(ActivePositionalArg, std::string_view(argv[i]),
                                i);
        continue;
      }
    } else {
      ArgName = std::string_view(argv[i] + 1);
      if (consume_front(ArgName, "-"))
        HaveDoubleDash = true;

      Handler = LookupLongOption(*ChosenSubCommand, ArgName, Value,
                                 LongOptionsUseDoubleDash, HaveDoubleDash);

      if (!Handler && ChosenSubCommand != &SubCommand::getTopLevel())
        Handler = LookupLongOption(SubCommand::getTopLevel(), ArgName, Value,
                                   LongOptionsUseDoubleDash, HaveDoubleDash);

      if (!Handler && !(LongOptionsUseDoubleDash && HaveDoubleDash))
        Handler = HandlePrefixedOrGroupedOption(ArgName, Value, ErrorParsing,
                                                OptionsMap);

      if (!Handler && SinkOpts.empty())
        LookupNearestOption(ArgName, OptionsMap, NearestHandlerString);
    }

    if (!Handler) {
      if (!SinkOpts.empty()) {
        for (Option *SinkOpt : SinkOpts)
          SinkOpt->addOccurrence(i, "", std::string_view(argv[i]));
        continue;
      }

      auto ReportUnknownArgument = [&](bool IsArg,
                                       std::string_view NearestArgumentName) {
        *Errs << ProgramName << ": Unknown "
              << (IsArg ? "command line argument" : "subcommand") << " '"
              << argv[i] << "'.  Try: '" << argv[0] << " --help'\n";

        if (NearestArgumentName.empty())
          return;

        *Errs << ProgramName << ": Did you mean '";
        if (IsArg)
          *Errs << PrintArg(NearestArgumentName, 0);
        else
          *Errs << NearestArgumentName;
        *Errs << "'?\n";
      };

      if (i > 1 || !MaybeNamedSubCommand)
        ReportUnknownArgument(/*IsArg=*/true, NearestHandlerString);
      else
        ReportUnknownArgument(/*IsArg=*/false, NearestSubCommandString);

      ErrorParsing = true;
      continue;
    }

    if (Handler->getFormattingFlag() == llcl::Positional) {
      if ((Handler->getMiscFlags() & PositionalEatsArgs) && !Value.empty()) {
        Handler->error("This argument does not take a value.\n"
                       "\tInstead, it consumes any positional arguments until "
                       "the next recognized option.",
                       *Errs);
        ErrorParsing = true;
      }
      ActivePositionalArg = Handler;
    } else
      ErrorParsing |= ProvideOption(Handler, ArgName, Value, argc, argv, i);
  }

  if (NumPositionalRequired > PositionalVals.size()) {
    *Errs << ProgramName
          << ": Not enough positional command line arguments specified!\n"
          << "Must specify at least " << NumPositionalRequired
          << " positional argument" << (NumPositionalRequired > 1 ? "s" : "")
          << ": See: " << argv[0] << " --help\n";

    ErrorParsing = true;
  } else if (!HasUnlimitedPositionals &&
             PositionalVals.size() > PositionalOpts.size()) {
    *Errs << ProgramName << ": Too many positional arguments specified!\n"
          << "Can specify at most " << PositionalOpts.size()
          << " positional arguments: See: " << argv[0] << " --help\n";
    ErrorParsing = true;

  } else if (!ConsumeAfterOpt) {
    unsigned ValNo = 0, NumVals = static_cast<unsigned>(PositionalVals.size());
    for (Option *Opt : PositionalOpts) {
      if (RequiresValue(Opt)) {
        ProvidePositionalOption(Opt, PositionalVals[ValNo].first,
                                PositionalVals[ValNo].second);
        ValNo++;
        --NumPositionalRequired;
      }

      bool Done = Opt->getNumOccurrencesFlag() == llcl::Required;
      while (NumVals - ValNo > NumPositionalRequired && !Done) {
        switch (Opt->getNumOccurrencesFlag()) {
        case llcl::Optional:
          Done = true;
          [[fallthrough]];
        case llcl::ZeroOrMore:
        case llcl::OneOrMore:
          ProvidePositionalOption(Opt, PositionalVals[ValNo].first,
                                  PositionalVals[ValNo].second);
          ValNo++;
          break;
        default:
          llcl_unreachable("Internal error, unexpected NumOccurrences flag in "
                           "positional argument processing!");
        }
      }
    }
  } else {
    assert(ConsumeAfterOpt && NumPositionalRequired <= PositionalVals.size());
    unsigned ValNo = 0;
    for (Option *Opt : PositionalOpts)
      if (RequiresValue(Opt)) {
        ErrorParsing |= ProvidePositionalOption(
            Opt, PositionalVals[ValNo].first, PositionalVals[ValNo].second);
        ValNo++;
      }

    if (PositionalOpts.size() == 1 && ValNo == 0 && !PositionalVals.empty()) {
      ErrorParsing |= ProvidePositionalOption(PositionalOpts[0],
                                              PositionalVals[ValNo].first,
                                              PositionalVals[ValNo].second);
      ValNo++;
    }

    for (; ValNo != PositionalVals.size(); ++ValNo)
      ErrorParsing |=
          ProvidePositionalOption(ConsumeAfterOpt, PositionalVals[ValNo].first,
                                  PositionalVals[ValNo].second);
  }

  for (const auto &Opt : OptionsMap) {
    switch (Opt.second->getNumOccurrencesFlag()) {
    case Required:
    case OneOrMore:
      if (Opt.second->getNumOccurrences() == 0) {
        Opt.second->error("must be specified at least once!");
        ErrorParsing = true;
      }
      [[fallthrough]];
    default:
      break;
    }
  }

  MoreHelp.clear();

  if (ErrorParsing) {
    if (!IgnoreErrors)
      exit(1);
    return false;
  }
  return true;
}

//===----------------------------------------------------------------------===//
// Option Base class implementation
//

bool Option::error(const std::string &Message, std::string_view ArgName,
                   std::ostream &Errs) {
  if (ArgName.empty())
    ArgName = ArgStr;
  if (ArgName.empty())
    Errs << HelpStr;
  else
    Errs << GlobalParser().ProgramName << ": for the " << PrintArg(ArgName, 0);

  Errs << " option: " << Message << "\n";
  return true;
}

bool Option::addOccurrence(unsigned pos, std::string_view ArgName,
                           std::string_view Value, bool MultiArg) {
  if (!MultiArg)
    NumOccurrences++;

  return handleOccurrence(pos, ArgName, Value);
}

// getValueStr
static std::string_view getValueStr(const Option &O,
                                    std::string_view DefaultMsg) {
  if (O.ValueStr.empty())
    return DefaultMsg;
  return O.ValueStr;
}

//===----------------------------------------------------------------------===//
// llcl::alias class implementation
//

size_t alias::getOptionWidth() const { return argPlusPrefixesSize(ArgStr); }

void Option::printHelpStr(std::string_view HelpStr, size_t Indent,
                          size_t FirstLineIndentedBy) {
  assert(Indent >= FirstLineIndentedBy);
  auto [First, Rest] = split(HelpStr, '\n');
  indent(std::cout, Indent - FirstLineIndentedBy);
  std::cout << ArgHelpPrefix << First << "\n";
  while (!Rest.empty()) {
    auto [Line, Remaining] = split(Rest, '\n');
    indent(std::cout, Indent);
    std::cout << Line << "\n";
    Rest = Remaining;
  }
}

void Option::printEnumValHelpStr(std::string_view HelpStr, size_t BaseIndent,
                                 size_t FirstLineIndentedBy) {
  const std::string_view ValHelpPrefix = "  ";
  assert(BaseIndent >= FirstLineIndentedBy);
  auto [First, Rest] = split(HelpStr, '\n');
  indent(std::cout, BaseIndent - FirstLineIndentedBy);
  std::cout << ArgHelpPrefix << ValHelpPrefix << First << "\n";
  while (!Rest.empty()) {
    auto [Line, Remaining] = split(Rest, '\n');
    indent(std::cout, BaseIndent + ValHelpPrefix.size());
    std::cout << Line << "\n";
    Rest = Remaining;
  }
}

void alias::printOptionInfo(size_t GlobalWidth) const {
  std::cout << PrintArg(ArgStr);
  printHelpStr(HelpStr, GlobalWidth, argPlusPrefixesSize(ArgStr));
}

//===----------------------------------------------------------------------===//
// Parser Implementation code...
//

size_t basic_parser_impl::getOptionWidth(const Option &O) const {
  size_t Len = argPlusPrefixesSize(O.ArgStr);
  auto ValName = getValueName();
  if (!ValName.empty()) {
    size_t FormattingLen = 3;
    if (O.getMiscFlags() & PositionalEatsArgs)
      FormattingLen = 6;
    Len += getValueStr(O, ValName).size() + FormattingLen;
  }

  return Len;
}

void basic_parser_impl::printOptionInfo(const Option &O,
                                        size_t GlobalWidth) const {
  std::cout << PrintArg(O.ArgStr);

  auto ValName = getValueName();
  if (!ValName.empty()) {
    if (O.getMiscFlags() & PositionalEatsArgs) {
      std::cout << " <" << getValueStr(O, ValName) << ">...";
    } else if (O.getValueExpectedFlag() == ValueOptional)
      std::cout << "[=<" << getValueStr(O, ValName) << ">]";
    else {
      std::cout << (O.ArgStr.size() == 1 ? " <" : "=<")
                << getValueStr(O, ValName) << '>';
    }
  }

  Option::printHelpStr(O.HelpStr, GlobalWidth, getOptionWidth(O));
}

void basic_parser_impl::printOptionName(const Option &O,
                                        size_t GlobalWidth) const {
  std::cout << PrintArg(O.ArgStr);
  indent(std::cout, GlobalWidth - O.ArgStr.size());
}

// parser<bool> implementation
bool parser<bool>::parse(Option &O, std::string_view ArgName,
                         std::string_view Arg, bool &Value) {
  return parseBool<bool, true, false>(O, ArgName, Arg, Value);
}

// parser<boolOrDefault> implementation
bool parser<boolOrDefault>::parse(Option &O, std::string_view ArgName,
                                  std::string_view Arg, boolOrDefault &Value) {
  return parseBool<boolOrDefault, BOU_TRUE, BOU_FALSE>(O, ArgName, Arg, Value);
}

// parser<int> implementation
bool parser<int>::parse(Option &O, [[maybe_unused]] std::string_view ArgName,
                        std::string_view Arg, int &Value) {
  if (getAsInteger(Arg, Value))
    return O.error("'" + std::string(Arg) +
                   "' value invalid for integer argument!");
  return false;
}

// parser<long> implementation
bool parser<long>::parse(Option &O, [[maybe_unused]] std::string_view ArgName,
                         std::string_view Arg, long &Value) {
  if (getAsInteger(Arg, Value))
    return O.error("'" + std::string(Arg) +
                   "' value invalid for long argument!");
  return false;
}

// parser<long long> implementation
bool parser<long long>::parse(Option &O,
                              [[maybe_unused]] std::string_view ArgName,
                              std::string_view Arg, long long &Value) {
  if (getAsInteger(Arg, Value))
    return O.error("'" + std::string(Arg) +
                   "' value invalid for llong argument!");
  return false;
}

// parser<unsigned> implementation
bool parser<unsigned>::parse(Option &O,
                             [[maybe_unused]] std::string_view ArgName,
                             std::string_view Arg, unsigned &Value) {
  if (getAsInteger(Arg, Value))
    return O.error("'" + std::string(Arg) +
                   "' value invalid for uint argument!");
  return false;
}

// parser<unsigned long> implementation
bool parser<unsigned long>::parse(Option &O,
                                  [[maybe_unused]] std::string_view ArgName,
                                  std::string_view Arg, unsigned long &Value) {
  if (getAsInteger(Arg, Value))
    return O.error("'" + std::string(Arg) +
                   "' value invalid for ulong argument!");
  return false;
}

// parser<unsigned long long> implementation
bool parser<unsigned long long>::parse(
    Option &O, [[maybe_unused]] std::string_view ArgName, std::string_view Arg,
    unsigned long long &Value) {
  if (getAsInteger(Arg, Value))
    return O.error("'" + std::string(Arg) +
                   "' value invalid for ullong argument!");
  return false;
}

// parser<double>/parser<float> implementation
static bool parseDouble(Option &O, std::string_view Arg, double &Value) {
  if (to_float(Arg, Value))
    return false;
  return O.error("'" + std::string(Arg) +
                 "' value invalid for floating point argument!");
}

bool parser<double>::parse(Option &O, [[maybe_unused]] std::string_view ArgName,
                           std::string_view Arg, double &Val) {
  return parseDouble(O, Arg, Val);
}

bool parser<float>::parse(Option &O, [[maybe_unused]] std::string_view ArgName,
                          std::string_view Arg, float &Val) {
  double dVal;
  if (parseDouble(O, Arg, dVal))
    return true;
  Val = (float)dVal;
  return false;
}

// generic_parser_base implementation

unsigned generic_parser_base::findOption(std::string_view Name) {
  unsigned e = getNumOptions();

  for (unsigned i = 0; i != e; ++i) {
    if (getOption(i) == Name)
      return i;
  }
  return e;
}

static std::string_view EqValue = "=<value>";
static std::string_view EmptyOption = "<empty>";
static std::string_view OptionPrefix = "    =";
static size_t getOptionPrefixesSize() {
  return OptionPrefix.size() + ArgHelpPrefix.size();
}

static bool shouldPrintOption(std::string_view Name,
                              std::string_view Description, const Option &O) {
  return O.getValueExpectedFlag() != ValueOptional || !Name.empty() ||
         !Description.empty();
}

size_t generic_parser_base::getOptionWidth(const Option &O) const {
  if (O.hasArgStr()) {
    size_t Size = argPlusPrefixesSize(O.ArgStr) + EqValue.size();
    for (unsigned i = 0, e = getNumOptions(); i != e; ++i) {
      std::string_view Name = getOption(i);
      if (!shouldPrintOption(Name, getDescription(i), O))
        continue;
      size_t NameSize = Name.empty() ? EmptyOption.size() : Name.size();
      Size = std::max(Size, NameSize + getOptionPrefixesSize());
    }
    return Size;
  } else {
    size_t BaseSize = 0;
    for (unsigned i = 0, e = getNumOptions(); i != e; ++i)
      BaseSize = std::max(BaseSize, getOption(i).size() + 8);
    return BaseSize;
  }
}

void generic_parser_base::printOptionInfo(const Option &O,
                                          size_t GlobalWidth) const {
  if (O.hasArgStr()) {
    if (O.getValueExpectedFlag() == ValueOptional) {
      for (unsigned i = 0, e = getNumOptions(); i != e; ++i) {
        if (getOption(i).empty()) {
          std::cout << PrintArg(O.ArgStr);
          Option::printHelpStr(O.HelpStr, GlobalWidth,
                               argPlusPrefixesSize(O.ArgStr));
          break;
        }
      }
    }

    std::cout << PrintArg(O.ArgStr) << EqValue;
    Option::printHelpStr(O.HelpStr, GlobalWidth,
                         EqValue.size() + argPlusPrefixesSize(O.ArgStr));
    for (unsigned i = 0, e = getNumOptions(); i != e; ++i) {
      std::string_view OptionName = getOption(i);
      std::string_view Description = getDescription(i);
      if (!shouldPrintOption(OptionName, Description, O))
        continue;
      size_t FirstLineIndent = OptionName.size() + getOptionPrefixesSize();
      std::cout << OptionPrefix << OptionName;
      if (OptionName.empty()) {
        std::cout << EmptyOption;
        assert(FirstLineIndent >= EmptyOption.size());
        FirstLineIndent += EmptyOption.size();
      }
      if (!Description.empty())
        Option::printEnumValHelpStr(Description, GlobalWidth, FirstLineIndent);
      else
        std::cout << '\n';
    }
  } else {
    if (!O.HelpStr.empty())
      std::cout << "  " << O.HelpStr << '\n';
    for (unsigned i = 0, e = getNumOptions(); i != e; ++i) {
      std::string_view OptName = getOption(i);
      std::cout << "    " << PrintArg(OptName);
      Option::printHelpStr(getDescription(i), GlobalWidth, OptName.size() + 8);
    }
  }
}

static const size_t MaxOptWidth = 8;

// printGenericOptionDiff
void generic_parser_base::printGenericOptionDiff(
    const Option &O, const GenericOptionValue &Value,
    const GenericOptionValue &Default, size_t GlobalWidth) const {
  std::cout << "  " << PrintArg(O.ArgStr);
  indent(std::cout, GlobalWidth - O.ArgStr.size());

  unsigned NumOpts = getNumOptions();
  for (unsigned i = 0; i != NumOpts; ++i) {
    if (!Value.compare(getOptionValue(i)))
      continue;

    std::cout << "= " << getOption(i);
    size_t L = getOption(i).size();
    size_t NumSpaces = MaxOptWidth > L ? MaxOptWidth - L : 0;
    indent(std::cout, NumSpaces);
    std::cout << " (default: ";
    for (unsigned j = 0; j != NumOpts; ++j) {
      if (!Default.compare(getOptionValue(j)))
        continue;
      std::cout << getOption(j);
      break;
    }
    std::cout << ")\n";
    return;
  }
  std::cout << "= *unknown option value*\n";
}

// printOptionDiff - Specializations for printing basic value types.
#define PRINT_OPT_DIFF(T)                                                      \
  void parser<T>::printOptionDiff(const Option &O, T V, OptionValue<T> D,      \
                                  size_t GlobalWidth) const {                  \
    printOptionName(O, GlobalWidth);                                           \
    std::ostringstream SS;                                                     \
    SS << V;                                                                   \
    std::string Str = SS.str();                                                \
    std::cout << "= " << Str;                                                  \
    size_t NumSpaces =                                                         \
        MaxOptWidth > Str.size() ? MaxOptWidth - Str.size() : 0;               \
    indent(std::cout, NumSpaces);                                              \
    std::cout << " (default: ";                                                \
    if (D.hasValue())                                                          \
      std::cout << D.getValue();                                               \
    else                                                                       \
      std::cout << "*no default*";                                             \
    std::cout << ")\n";                                                        \
  }

PRINT_OPT_DIFF(bool)
PRINT_OPT_DIFF(boolOrDefault)
PRINT_OPT_DIFF(int)
PRINT_OPT_DIFF(long)
PRINT_OPT_DIFF(long long)
PRINT_OPT_DIFF(unsigned)
PRINT_OPT_DIFF(unsigned long)
PRINT_OPT_DIFF(unsigned long long)
PRINT_OPT_DIFF(double)
PRINT_OPT_DIFF(float)
PRINT_OPT_DIFF(char)

void parser<std::string>::printOptionDiff(const Option &O, std::string_view V,
                                          const OptionValue<std::string> &D,
                                          size_t GlobalWidth) const {
  printOptionName(O, GlobalWidth);
  std::cout << "= " << V;
  size_t NumSpaces = MaxOptWidth > V.size() ? MaxOptWidth - V.size() : 0;
  indent(std::cout, NumSpaces);
  std::cout << " (default: ";
  if (D.hasValue())
    std::cout << D.getValue();
  else
    std::cout << "*no default*";
  std::cout << ")\n";
}

void parser<std::optional<std::string>>::printOptionDiff(
    const Option &O, std::optional<std::string_view> V,
    const OptionValue<std::optional<std::string>> &D,
    size_t GlobalWidth) const {
  printOptionName(O, GlobalWidth);
  if (V.has_value())
    std::cout << "= " << V.value();
  else
    std::cout << "= ";
  size_t VSize = V.has_value() ? V.value().size() : 0;
  size_t NumSpaces = MaxOptWidth > VSize ? MaxOptWidth - VSize : 0;
  indent(std::cout, NumSpaces);
  std::cout << " (default: ";
  if (D.hasValue() && D.getValue().has_value())
    std::cout << D.getValue().value();
  else
    std::cout << "*no value*";
  std::cout << ")\n";
}

void basic_parser_impl::printOptionNoValue(const Option &O,
                                           size_t GlobalWidth) const {
  printOptionName(O, GlobalWidth);
  std::cout << "= *cannot print option value*\n";
}

//===----------------------------------------------------------------------===//
// -help and -help-hidden option implementation
//

static int OptNameCompare(const std::pair<const char *, Option *> &LHS,
                          const std::pair<const char *, Option *> &RHS) {
  return std::strcmp(LHS.first, RHS.first);
}

static int SubNameCompare(const std::pair<const char *, SubCommand *> &LHS,
                          const std::pair<const char *, SubCommand *> &RHS) {
  return std::strcmp(LHS.first, RHS.first);
}

// Copy Options into a vector so we can sort them.
static void sortOpts(OptionsMapTy &OptMap,
                     std::vector<std::pair<const char *, Option *>> &Opts,
                     bool ShowHidden) {
  std::unordered_set<Option *> OptionSet;

  for (auto I = OptMap.begin(), E = OptMap.end(); I != E; ++I) {
    if (I->second->getOptionHiddenFlag() == ReallyHidden)
      continue;

    if (I->second->getOptionHiddenFlag() == Hidden && !ShowHidden)
      continue;

    if (!OptionSet.insert(I->second).second)
      continue;

    Opts.push_back(
        std::pair<const char *, Option *>(I->first.c_str(), I->second));
  }

  std::sort(Opts.begin(), Opts.end(), [](const auto &A, const auto &B) {
    return OptNameCompare(A, B) < 0;
  });
}

static void
sortSubCommands(const std::unordered_set<SubCommand *> &SubMap,
                std::vector<std::pair<const char *, SubCommand *>> &Subs) {
  for (auto *S : SubMap) {
    if (S->getName().empty())
      continue;
    Subs.push_back(std::make_pair(S->getName().data(), S));
  }
  std::sort(Subs.begin(), Subs.end(), [](const auto &A, const auto &B) {
    return SubNameCompare(A, B) < 0;
  });
}

namespace {

class HelpPrinter {
protected:
  const bool ShowHidden;
  using StrOptionPairVector = std::vector<std::pair<const char *, Option *>>;
  using StrSubCommandPairVector =
      std::vector<std::pair<const char *, SubCommand *>>;

  virtual void printOptions(StrOptionPairVector &Opts, size_t MaxArgLen) {
    for (const auto &Opt : Opts)
      Opt.second->printOptionInfo(MaxArgLen);
  }

  void printSubCommands(StrSubCommandPairVector &Subs, size_t MaxSubLen) {
    for (const auto &S : Subs) {
      std::cout << "  " << S.first;
      if (!S.second->getDescription().empty()) {
        indent(std::cout, MaxSubLen - std::strlen(S.first));
        std::cout << " - " << S.second->getDescription();
      }
      std::cout << "\n";
    }
  }

public:
  explicit HelpPrinter(bool showHidden) : ShowHidden(showHidden) {}
  virtual ~HelpPrinter() = default;

  void operator=(bool Value) {
    if (!Value)
      return;
    printHelp();

    exit(0);
  }

  void printHelp() {
    SubCommand *Sub = GlobalParser().getActiveSubCommand();
    auto &OptionsMap = Sub->OptionsMap;
    auto &PositionalOpts = Sub->PositionalOpts;
    auto &ConsumeAfterOpt = Sub->ConsumeAfterOpt;

    StrOptionPairVector Opts;
    sortOpts(OptionsMap, Opts, ShowHidden);

    StrSubCommandPairVector Subs;
    sortSubCommands(GlobalParser().RegisteredSubCommands, Subs);

    if (!GlobalParser().ProgramOverview.empty())
      std::cout << "OVERVIEW: " << GlobalParser().ProgramOverview << "\n";

    if (Sub == &SubCommand::getTopLevel()) {
      std::cout << "USAGE: " << GlobalParser().ProgramName;
      if (!Subs.empty())
        std::cout << " [subcommand]";
      std::cout << " [options]";
    } else {
      if (!Sub->getDescription().empty()) {
        std::cout << "SUBCOMMAND '" << Sub->getName()
                  << "': " << Sub->getDescription() << "\n\n";
      }
      std::cout << "USAGE: " << GlobalParser().ProgramName << " "
                << Sub->getName() << " [options]";
    }

    for (auto *Opt : PositionalOpts) {
      if (Opt->hasArgStr())
        std::cout << " --" << Opt->ArgStr;
      std::cout << " " << Opt->HelpStr;
    }

    if (ConsumeAfterOpt)
      std::cout << " " << ConsumeAfterOpt->HelpStr;

    if (Sub == &SubCommand::getTopLevel() && !Subs.empty()) {
      size_t MaxSubLen = 0;
      for (const auto &Sub : Subs)
        MaxSubLen = std::max(MaxSubLen, std::strlen(Sub.first));

      std::cout << "\n\n";
      std::cout << "SUBCOMMANDS:\n\n";
      printSubCommands(Subs, MaxSubLen);
      std::cout << "\n";
      std::cout << "  Type \"" << GlobalParser().ProgramName
                << " <subcommand> --help\" to get more help on a specific "
                   "subcommand";
    }

    std::cout << "\n\n";

    size_t MaxArgLen = 0;
    for (const auto &Opt : Opts)
      MaxArgLen = std::max(MaxArgLen, Opt.second->getOptionWidth());

    std::cout << "OPTIONS:\n";
    printOptions(Opts, MaxArgLen);

    for (const auto &I : GlobalParser().MoreHelp)
      std::cout << I;
    GlobalParser().MoreHelp.clear();
  }
};

class CategorizedHelpPrinter : public HelpPrinter {
public:
  explicit CategorizedHelpPrinter(bool showHidden) : HelpPrinter(showHidden) {}

  using HelpPrinter::operator=;

protected:
  void printOptions(StrOptionPairVector &Opts, size_t MaxArgLen) override {
    std::vector<OptionCategory *> SortedCategories;
    std::map<OptionCategory *, std::vector<Option *>> CategorizedOptions;

    for (auto *Cat : GlobalParser().RegisteredOptionCategories)
      SortedCategories.push_back(Cat);

    assert(SortedCategories.size() > 0 && "No option categories registered!");
    std::sort(SortedCategories.begin(), SortedCategories.end(),
              [](OptionCategory *A, OptionCategory *B) {
                return A->getName() < B->getName();
              });

    for (const auto &I : Opts) {
      Option *Opt = I.second;
      for (OptionCategory *Cat : Opt->Categories) {
        CategorizedOptions[Cat].push_back(Opt);
      }
    }

    for (OptionCategory *Category : SortedCategories) {
      const auto &CategoryOptions = CategorizedOptions[Category];
      if (CategoryOptions.empty())
        continue;

      std::cout << "\n";
      std::cout << Category->getName() << ":\n";

      if (!Category->getDescription().empty())
        std::cout << Category->getDescription() << "\n\n";
      else
        std::cout << "\n";

      for (const Option *Opt : CategoryOptions)
        Opt->printOptionInfo(MaxArgLen);
    }
  }
};

class HelpPrinterWrapper {
private:
  HelpPrinter &UncategorizedPrinter;
  CategorizedHelpPrinter &CategorizedPrinter;

public:
  explicit HelpPrinterWrapper(HelpPrinter &UncategorizedPrinter,
                              CategorizedHelpPrinter &CategorizedPrinter)
      : UncategorizedPrinter(UncategorizedPrinter),
        CategorizedPrinter(CategorizedPrinter) {}

  void operator=(bool Value);
};

class VersionPrinter {
public:
  void print(std::vector<VersionPrinterTy> ExtraPrinters = {}) {
    std::cout << "CommandLine Library:\n";
#ifndef NDEBUG
    std::cout << "  DEBUG build with assertions.\n";
#else
    std::cout << "  Release build.\n";
#endif

    if (!ExtraPrinters.empty()) {
      for (const auto &I : ExtraPrinters)
        I(std::cout);
    }
  }
  void operator=(bool OptionWasSpecified);
};

class ShellCompletionPrinter {
  // Helpers to sanitize identifiers for shell functions
  static std::string sanitizeForShell(std::string_view Name) {
    std::string Result;
    Result.reserve(Name.size());
    for (char C : Name) {
      if (C == '-' || C == '.')
        Result.push_back('_');
      else if ((C >= 'a' && C <= 'z') || (C >= 'A' && C <= 'Z') ||
               (C >= '0' && C <= '9') || C == '_')
        Result.push_back(C);
    }
    return Result;
  }

  // Escape single quotes for shell strings
  static std::string escapeForShell(std::string_view Str) {
    std::string Result;
    for (char C : Str) {
      if (C == '\'') {
        Result += "'\\''";
      } else {
        Result.push_back(C);
      }
    }
    return Result;
  }

  // Escape special chars for zsh descriptions
  static std::string escapeForZsh(std::string_view Str) {
    std::string Result;
    for (char C : Str) {
      if (C == '[' || C == ']' || C == ':' || C == '\\' || C == '\'')
        Result.push_back('\\');
      Result.push_back(C);
    }
    return Result;
  }

  struct OptionInfo {
    std::string Name;
    std::string Description;
    std::vector<std::string> Values;
    bool IsPositional;
  };

  // Collect options for a given subcommand
  static std::vector<OptionInfo> collectOptions(SubCommand &Sub) {
    std::vector<OptionInfo> Result;
    std::unordered_set<Option *> Seen;

    for (auto &[Key, Opt] : Sub.OptionsMap) {
      if (Opt->getOptionHiddenFlag() == ReallyHidden)
        continue;
      if (!Seen.insert(Opt).second)
        continue;

      OptionInfo Info;
      Info.Name = std::string(Opt->ArgStr);
      Info.Description = std::string(Opt->HelpStr);
      Info.IsPositional = Opt->isPositional();

      std::vector<std::string_view> CompValues;
      Opt->getOptionCompletionValues(CompValues);
      for (auto V : CompValues)
        Info.Values.push_back(std::string(V));

      Result.push_back(std::move(Info));
    }

    // Sort for deterministic output
    std::sort(Result.begin(), Result.end(),
              [](const OptionInfo &A, const OptionInfo &B) {
                return A.Name < B.Name;
              });
    return Result;
  }

  // Collect named subcommands
  static std::vector<std::pair<std::string, std::string>> collectSubCommands() {
    std::vector<std::pair<std::string, std::string>> Result;
    for (auto *S : GlobalParser().RegisteredSubCommands) {
      if (S->getName().empty())
        continue;
      Result.emplace_back(std::string(S->getName()),
                          std::string(S->getDescription()));
    }
    std::sort(Result.begin(), Result.end());
    return Result;
  }

public:
  void operator=(const std::string &Value) {
    if (Value.empty())
      return;

    if (Value == "bash")
      printBashCompletion();
    else if (Value == "zsh")
      printZshCompletion();
    else {
      llcl::errs() << "Unsupported shell: " << Value
                   << ". Supported: bash, zsh\n";
      exit(1);
    }
    exit(0);
  }

  void printBashCompletion() {
    auto &OS = llcl::outs();
    std::string ProgName = GlobalParser().ProgramName;
    std::string FuncName = "_" + sanitizeForShell(ProgName) + "_completion";

    auto SubCmds = collectSubCommands();
    bool HasSubCmds = !SubCmds.empty();

    OS << "# bash completion for " << ProgName << "\n";
    OS << FuncName << "() {\n";
    OS << "    local cur prev words cword\n";
    OS << "    if type _init_completion &>/dev/null; then\n";
    OS << "        _init_completion || return\n";
    OS << "    else\n";
    OS << "        cur=\"${COMP_WORDS[COMP_CWORD]}\"\n";
    OS << "        prev=\"${COMP_WORDS[COMP_CWORD-1]}\"\n";
    OS << "        words=(\"${COMP_WORDS[@]}\")\n";
    OS << "        cword=$COMP_CWORD\n";
    OS << "    fi\n\n";

    if (HasSubCmds) {
      // Build subcommand detection
      OS << "    local subcmd=\"\"\n";
      OS << "    for ((i=1; i < cword; i++)); do\n";
      OS << "        case \"${words[i]}\" in\n";
      OS << "            ";
      for (size_t i = 0; i < SubCmds.size(); ++i) {
        if (i > 0)
          OS << "|";
        OS << SubCmds[i].first;
      }
      OS << ") subcmd=\"${words[i]}\"; break ;;\n";
      OS << "        esac\n";
      OS << "    done\n\n";
    }

    // Build value completion for prev option
    auto printValueCase = [&](const std::vector<OptionInfo> &Opts) {
      bool HasValueOpts = false;
      for (auto &Opt : Opts) {
        if (!Opt.Values.empty() && !Opt.IsPositional) {
          HasValueOpts = true;
          break;
        }
      }
      if (!HasValueOpts)
        return;

      OS << "    case \"$prev\" in\n";
      for (auto &Opt : Opts) {
        if (Opt.Values.empty() || Opt.IsPositional)
          continue;
        std::string Prefix = Opt.Name.size() == 1 ? "-" : "--";
        OS << "        " << Prefix << Opt.Name << ")\n";
        OS << "            COMPREPLY=($(compgen -W '";
        for (size_t i = 0; i < Opt.Values.size(); ++i) {
          if (i > 0)
            OS << " ";
          OS << Opt.Values[i];
        }
        OS << "' -- \"$cur\")); return ;;\n";
      }
      OS << "    esac\n\n";
    };

    auto printOptsBlock = [&](const std::vector<OptionInfo> &Opts,
                              bool IncludeSubCmds) {
      OS << "    local opts=\"";
      bool First = true;
      for (auto &Opt : Opts) {
        if (Opt.IsPositional)
          continue;
        if (!First)
          OS << " ";
        First = false;
        OS << (Opt.Name.size() == 1 ? "-" : "--") << Opt.Name;
      }
      if (IncludeSubCmds) {
        for (auto &SC : SubCmds) {
          if (!First)
            OS << " ";
          First = false;
          OS << SC.first;
        }
      }
      OS << "\"\n";
      OS << "    COMPREPLY=($(compgen -W \"$opts\" -- \"$cur\"))\n";
    };

    if (HasSubCmds) {
      // Per-subcommand completion
      OS << "    case \"$subcmd\" in\n";
      for (auto &SC : SubCmds) {
        // Find the SubCommand* to collect its options
        SubCommand *SCPtr = nullptr;
        for (auto *S : GlobalParser().RegisteredSubCommands) {
          if (S->getName() == SC.first) {
            SCPtr = S;
            break;
          }
        }
        if (!SCPtr)
          continue;

        auto Opts = collectOptions(*SCPtr);
        OS << "        " << SC.first << ")\n";
        // Print value completion for this subcommand
        bool HasValueOpts = false;
        for (auto &Opt : Opts) {
          if (!Opt.Values.empty() && !Opt.IsPositional) {
            HasValueOpts = true;
            break;
          }
        }
        if (HasValueOpts) {
          OS << "            case \"$prev\" in\n";
          for (auto &Opt : Opts) {
            if (Opt.Values.empty() || Opt.IsPositional)
              continue;
            std::string Prefix = Opt.Name.size() == 1 ? "-" : "--";
            OS << "                " << Prefix << Opt.Name << ")\n";
            OS << "                    COMPREPLY=($(compgen -W '";
            for (size_t i = 0; i < Opt.Values.size(); ++i) {
              if (i > 0)
                OS << " ";
              OS << Opt.Values[i];
            }
            OS << "' -- \"$cur\")); return ;;\n";
          }
          OS << "            esac\n";
        }
        OS << "            opts=\"";
        bool First = true;
        for (auto &Opt : Opts) {
          if (Opt.IsPositional)
            continue;
          if (!First)
            OS << " ";
          First = false;
          OS << (Opt.Name.size() == 1 ? "-" : "--") << Opt.Name;
        }
        OS << "\"\n";
        OS << "            COMPREPLY=($(compgen -W \"$opts\" -- \"$cur\"))\n";
        OS << "            return ;;\n";
      }
      OS << "    esac\n\n";

      // Top-level options
      auto TopOpts = collectOptions(SubCommand::getTopLevel());
      printValueCase(TopOpts);
      printOptsBlock(TopOpts, /*IncludeSubCmds=*/true);
    } else {
      auto TopOpts = collectOptions(SubCommand::getTopLevel());
      printValueCase(TopOpts);
      printOptsBlock(TopOpts, /*IncludeSubCmds=*/false);
    }

    OS << "}\n";
    OS << "complete -F " << FuncName << " " << ProgName << "\n";
  }

  void printZshCompletion() {
    auto &OS = llcl::outs();
    std::string ProgName = GlobalParser().ProgramName;
    std::string SafeName = sanitizeForShell(ProgName);

    auto SubCmds = collectSubCommands();
    bool HasSubCmds = !SubCmds.empty();

    OS << "#compdef " << ProgName << "\n\n";

    if (HasSubCmds) {
      // Main function dispatches to subcommands
      OS << "_" << SafeName << "() {\n";
      OS << "    local -a _subcommands\n";
      OS << "    _subcommands=(\n";
      for (auto &SC : SubCmds) {
        OS << "        '" << SC.first << ":" << escapeForZsh(SC.second)
           << "'\n";
      }
      OS << "    )\n\n";

      // Top-level options
      auto TopOpts = collectOptions(SubCommand::getTopLevel());
      OS << "    _arguments -C \\\n";
      for (auto &Opt : TopOpts) {
        if (Opt.IsPositional)
          continue;
        std::string Prefix = Opt.Name.size() == 1 ? "-" : "--";
        OS << "        '" << Prefix << Opt.Name;
        if (!Opt.Description.empty())
          OS << "[" << escapeForZsh(Opt.Description) << "]";
        if (!Opt.Values.empty()) {
          OS << ": :(";
          for (size_t i = 0; i < Opt.Values.size(); ++i) {
            if (i > 0)
              OS << " ";
            OS << Opt.Values[i];
          }
          OS << ")";
        }
        OS << "' \\\n";
      }
      OS << "        '1:subcommand:->subcmd' \\\n";
      OS << "        '*::arg:->args'\n\n";

      OS << "    case $state in\n";
      OS << "        subcmd)\n";
      OS << "            _describe 'subcommand' _subcommands\n";
      OS << "            ;;\n";
      OS << "        args)\n";
      OS << "            case $words[1] in\n";
      for (auto &SC : SubCmds) {
        OS << "                " << SC.first << ") _" << SafeName << "_"
           << sanitizeForShell(SC.first) << " ;;\n";
      }
      OS << "            esac\n";
      OS << "            ;;\n";
      OS << "    esac\n";
      OS << "}\n\n";

      // Per-subcommand functions
      for (auto &SC : SubCmds) {
        SubCommand *SCPtr = nullptr;
        for (auto *S : GlobalParser().RegisteredSubCommands) {
          if (S->getName() == SC.first) {
            SCPtr = S;
            break;
          }
        }
        if (!SCPtr)
          continue;

        auto Opts = collectOptions(*SCPtr);
        OS << "_" << SafeName << "_" << sanitizeForShell(SC.first) << "() {\n";
        OS << "    _arguments \\\n";
        for (auto &Opt : Opts) {
          if (Opt.IsPositional)
            continue;
          std::string Prefix = Opt.Name.size() == 1 ? "-" : "--";
          OS << "        '" << Prefix << Opt.Name;
          if (!Opt.Description.empty())
            OS << "[" << escapeForZsh(Opt.Description) << "]";
          if (!Opt.Values.empty()) {
            OS << ": :(";
            for (size_t i = 0; i < Opt.Values.size(); ++i) {
              if (i > 0)
                OS << " ";
              OS << Opt.Values[i];
            }
            OS << ")";
          }
          OS << "' \\\n";
        }
        OS << "        '*:file:_files'\n";
        OS << "}\n\n";
      }
    } else {
      // No subcommands - simple _arguments
      auto TopOpts = collectOptions(SubCommand::getTopLevel());
      OS << "_" << SafeName << "() {\n";
      OS << "    _arguments \\\n";
      for (auto &Opt : TopOpts) {
        if (Opt.IsPositional)
          continue;
        std::string Prefix = Opt.Name.size() == 1 ? "-" : "--";
        OS << "        '" << Prefix << Opt.Name;
        if (!Opt.Description.empty())
          OS << "[" << escapeForZsh(Opt.Description) << "]";
        if (!Opt.Values.empty()) {
          OS << ": :(";
          for (size_t i = 0; i < Opt.Values.size(); ++i) {
            if (i > 0)
              OS << " ";
            OS << Opt.Values[i];
          }
          OS << ")";
        }
        OS << "' \\\n";
      }
      OS << "        '*:file:_files'\n";
      OS << "}\n\n";
    }

    OS << "_" << SafeName << "\n";
  }
};

struct CommandLineCommonOptions {
  HelpPrinter UncategorizedNormalPrinter{false};
  HelpPrinter UncategorizedHiddenPrinter{true};
  CategorizedHelpPrinter CategorizedNormalPrinter{false};
  CategorizedHelpPrinter CategorizedHiddenPrinter{true};
  HelpPrinterWrapper WrappedNormalPrinter{UncategorizedNormalPrinter,
                                          CategorizedNormalPrinter};
  HelpPrinterWrapper WrappedHiddenPrinter{UncategorizedHiddenPrinter,
                                          CategorizedHiddenPrinter};
  llcl::OptionCategory GenericCategory{"Generic Options"};

  llcl::opt<HelpPrinter, true, parser<bool>> HLOp{
      "help-list",
      llcl::desc(
          "Display list of available options (--help-list-hidden for more)"),
      llcl::location(UncategorizedNormalPrinter),
      llcl::Hidden,
      llcl::ValueDisallowed,
      llcl::cat(GenericCategory),
      llcl::sub(SubCommand::getAll())};

  llcl::opt<HelpPrinter, true, parser<bool>> HLHOp{
      "help-list-hidden",
      llcl::desc("Display list of all available options"),
      llcl::location(UncategorizedHiddenPrinter),
      llcl::Hidden,
      llcl::ValueDisallowed,
      llcl::cat(GenericCategory),
      llcl::sub(SubCommand::getAll())};

  llcl::opt<HelpPrinterWrapper, true, parser<bool>> HOp{
      "help",
      llcl::desc("Display available options (--help-hidden for more)"),
      llcl::location(WrappedNormalPrinter),
      llcl::ValueDisallowed,
      llcl::cat(GenericCategory),
      llcl::sub(SubCommand::getAll())};

  llcl::alias HOpA{"h", llcl::desc("Alias for --help"), llcl::aliasopt(HOp),
                   llcl::DefaultOption};

  llcl::opt<HelpPrinterWrapper, true, parser<bool>> HHOp{
      "help-hidden",
      llcl::desc("Display all available options"),
      llcl::location(WrappedHiddenPrinter),
      llcl::Hidden,
      llcl::ValueDisallowed,
      llcl::cat(GenericCategory),
      llcl::sub(SubCommand::getAll())};

  llcl::opt<bool> PrintOptions{
      "print-options",
      llcl::desc("Print non-default options after command line parsing"),
      llcl::Hidden,
      llcl::init(false),
      llcl::cat(GenericCategory),
      llcl::sub(SubCommand::getAll())};

  llcl::opt<bool> PrintAllOptions{
      "print-all-options",
      llcl::desc("Print all option values after command line parsing"),
      llcl::Hidden,
      llcl::init(false),
      llcl::cat(GenericCategory),
      llcl::sub(SubCommand::getAll())};

  ShellCompletionPrinter CompletionPrinter;

  llcl::opt<ShellCompletionPrinter, true, parser<std::string>> CompletionOp{
      "generate-completion",
      llcl::desc("Generate shell completion script (bash or zsh)"),
      llcl::location(CompletionPrinter),
      llcl::ValueRequired,
      llcl::Hidden,
      llcl::cat(GenericCategory),
      llcl::sub(SubCommand::getAll())};

  VersionPrinterTy OverrideVersionPrinter = nullptr;

  std::vector<VersionPrinterTy> ExtraVersionPrinters;

  VersionPrinter VersionPrinterInstance;

  llcl::opt<VersionPrinter, true, parser<bool>> VersOp{
      "version", llcl::desc("Display the version of this program"),
      llcl::location(VersionPrinterInstance), llcl::ValueDisallowed,
      llcl::cat(GenericCategory)};
};
} // End anonymous namespace

// Meyer's singleton for CommonOptions
static CommandLineCommonOptions &CommonOptions() {
  static CommandLineCommonOptions Opts;
  return Opts;
}

static void initCommonOptions() { (void)CommonOptions(); }

OptionCategory &llcl::getGeneralCategory() {
  static OptionCategory GeneralCategory{"General options"};
  return GeneralCategory;
}

void VersionPrinter::operator=(bool OptionWasSpecified) {
  if (!OptionWasSpecified)
    return;

  if (CommonOptions().OverrideVersionPrinter != nullptr) {
    CommonOptions().OverrideVersionPrinter(std::cout);
    exit(0);
  }
  print(CommonOptions().ExtraVersionPrinters);

  exit(0);
}

void HelpPrinterWrapper::operator=(bool Value) {
  if (!Value)
    return;

  if (GlobalParser().RegisteredOptionCategories.size() > 1) {
    CommonOptions().HLOp.setHiddenFlag(NotHidden);

    CategorizedPrinter = true;
  } else {
    UncategorizedPrinter = true;
  }
}

void llcl::PrintOptionValues() { GlobalParser().printOptionValues(); }

void CommandLineParser::printOptionValues() {
  if (!CommonOptions().PrintOptions && !CommonOptions().PrintAllOptions)
    return;

  std::vector<std::pair<const char *, Option *>> Opts;
  sortOpts(ActiveSubCommand->OptionsMap, Opts, /*ShowHidden*/ true);

  size_t MaxArgLen = 0;
  for (const auto &Opt : Opts)
    MaxArgLen = std::max(MaxArgLen, Opt.second->getOptionWidth());

  for (const auto &Opt : Opts)
    Opt.second->printOptionValue(MaxArgLen, CommonOptions().PrintAllOptions);
}

void llcl::PrintHelpMessage(bool Hidden, bool Categorized) {
  if (!Hidden && !Categorized)
    CommonOptions().UncategorizedNormalPrinter.printHelp();
  else if (!Hidden && Categorized)
    CommonOptions().CategorizedNormalPrinter.printHelp();
  else if (Hidden && !Categorized)
    CommonOptions().UncategorizedHiddenPrinter.printHelp();
  else
    CommonOptions().CategorizedHiddenPrinter.printHelp();
}

void llcl::PrintVersionMessage() {
  CommonOptions().VersionPrinterInstance.print(
      CommonOptions().ExtraVersionPrinters);
}

void llcl::PrintShellCompletion(std::string_view Shell) {
  initCommonOptions();
  if (Shell == "bash")
    CommonOptions().CompletionPrinter.printBashCompletion();
  else if (Shell == "zsh")
    CommonOptions().CompletionPrinter.printZshCompletion();
}

void llcl::SetVersionPrinter(VersionPrinterTy func) {
  CommonOptions().OverrideVersionPrinter = func;
}

void llcl::AddExtraVersionPrinter(VersionPrinterTy func) {
  CommonOptions().ExtraVersionPrinters.push_back(func);
}

std::map<std::string, Option *, std::less<>> &
llcl::getRegisteredOptions(SubCommand &Sub) {
  initCommonOptions();
  return Sub.OptionsMap;
}

std::unordered_set<SubCommand *> &llcl::getRegisteredSubcommands() {
  return GlobalParser().getRegisteredSubcommands();
}

void llcl::HideUnrelatedOptions(llcl::OptionCategory &Category,
                                SubCommand &Sub) {
  initCommonOptions();
  for (auto &I : Sub.OptionsMap) {
    bool Unrelated = true;
    for (auto &Cat : I.second->Categories) {
      if (Cat == &Category || Cat == &CommonOptions().GenericCategory)
        Unrelated = false;
    }
    if (Unrelated)
      I.second->setHiddenFlag(llcl::ReallyHidden);
  }
}

void llcl::HideUnrelatedOptions(
    const std::vector<const llcl::OptionCategory *> &Categories,
    SubCommand &Sub) {
  initCommonOptions();
  for (auto &I : Sub.OptionsMap) {
    bool Unrelated = true;
    for (auto &Cat : I.second->Categories) {
      if (std::find(Categories.begin(), Categories.end(), Cat) !=
              Categories.end() ||
          Cat == &CommonOptions().GenericCategory)
        Unrelated = false;
    }
    if (Unrelated)
      I.second->setHiddenFlag(llcl::ReallyHidden);
  }
}

void llcl::ResetCommandLineParser() { GlobalParser().reset(); }
void llcl::ResetAllOptionOccurrences() {
  GlobalParser().ResetAllOptionOccurrences();
}

// Removed: C API (LLVMParseCommandLineOptions)
// Removed: getCompilerBuildConfig, printBuildConfig
