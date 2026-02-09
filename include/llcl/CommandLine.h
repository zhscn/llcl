//===- llvm/Support/CommandLine.h - Command line handler --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Standalone C++17 extraction of the LLVM CommandLine library.
// All LLVM types replaced with std:: equivalents.
//
//===----------------------------------------------------------------------===//

#ifndef LLCL_SUPPORT_COMMANDLINE_H
#define LLCL_SUPPORT_COMMANDLINE_H

#include <algorithm>
#include <cassert>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <deque>
#include <functional>
#include <initializer_list>
#include <iostream>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_set>
#include <vector>

namespace llcl {

//===----------------------------------------------------------------------===//
// Utility replacements for LLVM infrastructure
//===----------------------------------------------------------------------===//

/// Simple error type replacing llcl::Error
struct Error {
  std::string Message;
  bool IsError = false;

  Error() = default;
  Error(const std::string &Msg) : Message(Msg), IsError(true) {}
  Error(Error &&Other) noexcept
      : Message(std::move(Other.Message)), IsError(Other.IsError) {
    Other.IsError = false;
  }
  Error &operator=(Error &&Other) noexcept {
    Message = std::move(Other.Message);
    IsError = Other.IsError;
    Other.IsError = false;
    return *this;
  }

  static Error success() { return Error(); }
  explicit operator bool() const { return IsError; }
};

inline std::string toString(Error E) { return std::move(E.Message); }

inline Error createStringError(std::error_code /*EC*/, const std::string &Msg) {
  return Error(Msg);
}

template <typename... Args>
Error createStringError(std::error_code /*EC*/, const char *Fmt, Args &&...) {
  return Error(Fmt);
}

/// ErrorOr - a simple replacement
template <typename T> class ErrorOr {
  std::optional<T> Val;
  std::error_code EC;

public:
  ErrorOr(T V) : Val(std::move(V)) {}
  ErrorOr(std::error_code E) : EC(E) {}

  explicit operator bool() const { return Val.has_value(); }
  T &get() { return *Val; }
  const T &get() const { return *Val; }
  T &operator*() { return *Val; }
  std::error_code getError() const { return EC; }
};

/// StringSaver - saves strings with stable pointers using std::deque
class BumpPtrAllocator {}; // placeholder

class StringSaver {
  std::deque<std::string> Storage;

public:
  StringSaver() = default;
  explicit StringSaver(BumpPtrAllocator &) {}

  BumpPtrAllocator &getAllocator() {
    static BumpPtrAllocator A;
    return A;
  }

  std::string_view save(std::string_view S) {
    Storage.emplace_back(S);
    return std::string_view(Storage.back());
  }
};

/// report_fatal_error replacement
[[noreturn]] inline void report_fatal_error(std::string_view Msg) {
  std::cerr << "FATAL ERROR: " << Msg << "\n";
  std::abort();
}

/// Stream helpers replacing raw_ostream
inline std::ostream &outs() { return std::cout; }
inline std::ostream &errs() { return std::cerr; }

/// indent helper
inline std::ostream &indent(std::ostream &OS, size_t N) {
  for (size_t I = 0; I < N; ++I)
    OS << ' ';
  return OS;
}

/// llcl_unreachable replacement
#define llcl_unreachable(msg)                                                  \
  do {                                                                         \
    assert(false && msg);                                                      \
    __builtin_unreachable();                                                   \
  } while (0)

/// This namespace contains all of the command line option processing machinery.
/// It is intentionally a short name to make qualified usage concise.

//===----------------------------------------------------------------------===//
// Command line option processing entry point.
//
// Returns true on success. Otherwise, this will print the error message to
// stderr and exit if \p Errs is not set (nullptr by default), or print the
// error message to \p Errs and return false if \p Errs is provided.
bool ParseCommandLineOptions(int argc, const char *const *argv,
                             std::string_view Overview = "",
                             std::ostream *Errs = nullptr,
                             const char *EnvVar = nullptr,
                             bool LongOptionsUseDoubleDash = false);

// Function pointer type for printing version information.
using VersionPrinterTy = std::function<void(std::ostream &)>;

///===---------------------------------------------------------------------===//
/// Override the default version printer.
void SetVersionPrinter(VersionPrinterTy func);

///===---------------------------------------------------------------------===//
/// Add an extra printer to use in addition to the default one.
void AddExtraVersionPrinter(VersionPrinterTy func);

// Print option values.
void PrintOptionValues();

// Forward declaration
class Option;

/// Adds a new option for parsing and provides the option it refers to.
void AddLiteralOption(Option &O, std::string_view Name);

//===----------------------------------------------------------------------===//
// Flags permitted to be passed to command line arguments
//

enum NumOccurrencesFlag { // Flags for the number of occurrences allowed
  Optional = 0x00,        // Zero or One occurrence
  ZeroOrMore = 0x01,      // Zero or more occurrences allowed
  Required = 0x02,        // One occurrence required
  OneOrMore = 0x03,       // One or more occurrences required
  ConsumeAfter = 0x04
};

enum ValueExpected { // Is a value required for the option?
  // zero reserved for the unspecified value
  ValueOptional = 0x01,  // The value can appear... or not
  ValueRequired = 0x02,  // The value is required to appear!
  ValueDisallowed = 0x03 // A value may not be specified (for flags)
};

enum OptionHidden {   // Control whether -help shows this option
  NotHidden = 0x00,   // Option included in -help & -help-hidden
  Hidden = 0x01,      // -help doesn't, but -help-hidden does
  ReallyHidden = 0x02 // Neither -help nor -help-hidden show this arg
};

enum FormattingFlags {
  NormalFormatting = 0x00, // Nothing special
  Positional = 0x01,       // Is a positional argument, no '-' required
  Prefix = 0x02,           // Can this option directly prefix its value?
  AlwaysPrefix = 0x03      // Can this option only directly prefix its value?
};

enum MiscFlags {             // Miscellaneous flags to adjust argument
  CommaSeparated = 0x01,     // Should this list split between commas?
  PositionalEatsArgs = 0x02, // Should this positional list eat -args?
  Sink = 0x04,               // Should this list eat all unknown options?
  Grouping = 0x08,
  DefaultOption = 0x10
};

//===----------------------------------------------------------------------===//
//
class OptionCategory {
private:
  std::string_view const Name;
  std::string_view const Description;

  void registerCategory();

public:
  OptionCategory(std::string_view const Name,
                 std::string_view const Description = "")
      : Name(Name), Description(Description) {
    registerCategory();
  }

  std::string_view getName() const { return Name; }
  std::string_view getDescription() const { return Description; }
};

// The general Option Category (used as default category).
OptionCategory &getGeneralCategory();

//===----------------------------------------------------------------------===//
//
class SubCommand {
private:
  std::string_view Name;
  std::string_view Description;

protected:
  void registerSubCommand();
  void unregisterSubCommand();

public:
  SubCommand(std::string_view Name, std::string_view Description = "")
      : Name(Name), Description(Description) {
    registerSubCommand();
  }
  SubCommand() = default;

  // Get the special subcommand representing no subcommand.
  static SubCommand &getTopLevel();

  // Get the special subcommand that can be used to put an option into all
  // subcommands.
  static SubCommand &getAll();

  void reset();

  explicit operator bool() const;

  std::string_view getName() const { return Name; }
  std::string_view getDescription() const { return Description; }

  std::vector<Option *> PositionalOpts;
  std::vector<Option *> SinkOpts;
  std::map<std::string, Option *, std::less<>> OptionsMap;

  Option *ConsumeAfterOpt = nullptr; // The ConsumeAfter option if it exists.
};

class SubCommandGroup {
  std::vector<SubCommand *> Subs;

public:
  SubCommandGroup(std::initializer_list<SubCommand *> IL) : Subs(IL) {}

  const std::vector<SubCommand *> &getSubCommands() const { return Subs; }
};

//===----------------------------------------------------------------------===//
//
class Option {
  friend class alias;

  // Overriden by subclasses to handle the value passed into an argument. Should
  // return true if there was an error processing the argument and the program
  // should exit.
  //
  virtual bool handleOccurrence(unsigned pos, std::string_view ArgName,
                                std::string_view Arg) = 0;

  virtual enum ValueExpected getValueExpectedFlagDefault() const {
    return ValueOptional;
  }

  // Out of line virtual function to provide home for the class.
  virtual void anchor();

  uint16_t NumOccurrences; // The number of times specified
  // Occurrences, HiddenFlag, and Formatting are all enum types but to avoid
  // problems with signed enums in bitfields.
  uint16_t Occurrences : 3; // enum NumOccurrencesFlag
  // not using the enum type for 'Value' because zero is an implementation
  // detail representing the non-value
  uint16_t Value : 2;
  uint16_t HiddenFlag : 2; // enum OptionHidden
  uint16_t Formatting : 2; // enum FormattingFlags
  uint16_t Misc : 5;
  uint16_t FullyInitialized : 1; // Has addArgument been called?
  uint16_t Position;             // Position of last occurrence of the option
  uint16_t AdditionalVals;       // Greater than 0 for multi-valued option.

public:
  std::string_view ArgStr;  // The argument string itself (ex: "help", "o")
  std::string_view HelpStr; // The descriptive text message for -help
  std::string_view
      ValueStr; // String describing what the value of this option is
  std::vector<OptionCategory *>
      Categories; // The Categories this option belongs to
  std::unordered_set<SubCommand *>
      Subs; // The subcommands this option belongs to.

  inline enum NumOccurrencesFlag getNumOccurrencesFlag() const {
    return (enum NumOccurrencesFlag)Occurrences;
  }

  inline enum ValueExpected getValueExpectedFlag() const {
    return Value ? ((enum ValueExpected)Value) : getValueExpectedFlagDefault();
  }

  inline enum OptionHidden getOptionHiddenFlag() const {
    return (enum OptionHidden)HiddenFlag;
  }

  inline enum FormattingFlags getFormattingFlag() const {
    return (enum FormattingFlags)Formatting;
  }

  inline unsigned getMiscFlags() const { return Misc; }
  inline unsigned getPosition() const { return Position; }
  inline unsigned getNumAdditionalVals() const { return AdditionalVals; }

  // Return true if the argstr != ""
  bool hasArgStr() const { return !ArgStr.empty(); }
  bool isPositional() const { return getFormattingFlag() == Positional; }
  bool isSink() const { return getMiscFlags() & Sink; }
  bool isDefaultOption() const { return getMiscFlags() & DefaultOption; }

  bool isConsumeAfter() const {
    return getNumOccurrencesFlag() == ConsumeAfter;
  }

  //-------------------------------------------------------------------------===
  // Accessor functions set by OptionModifiers
  //
  void setArgStr(std::string_view S);
  void setDescription(std::string_view S) { HelpStr = S; }
  void setValueStr(std::string_view S) { ValueStr = S; }
  void setNumOccurrencesFlag(enum NumOccurrencesFlag Val) { Occurrences = Val; }
  void setValueExpectedFlag(enum ValueExpected Val) { Value = Val; }
  void setHiddenFlag(enum OptionHidden Val) { HiddenFlag = Val; }
  void setFormattingFlag(enum FormattingFlags V) { Formatting = V; }
  void setMiscFlag(enum MiscFlags M) { Misc |= M; }
  void setPosition(unsigned pos) { Position = pos; }
  void addCategory(OptionCategory &C);
  void addSubCommand(SubCommand &S) { Subs.insert(&S); }

protected:
  explicit Option(enum NumOccurrencesFlag OccurrencesFlag,
                  enum OptionHidden Hidden)
      : NumOccurrences(0), Occurrences(OccurrencesFlag), Value(0),
        HiddenFlag(Hidden), Formatting(NormalFormatting), Misc(0),
        FullyInitialized(false), Position(0), AdditionalVals(0) {
    Categories.push_back(&getGeneralCategory());
  }

  inline void setNumAdditionalVals(unsigned n) { AdditionalVals = n; }

public:
  virtual ~Option() = default;

  // Register this argument with the commandline system.
  //
  void addArgument();

  /// Unregisters this option from the CommandLine system.
  ///
  /// This option must have been the last option registered.
  /// For testing purposes only.
  void removeArgument();

  // Return the width of the option tag for printing...
  virtual size_t getOptionWidth() const = 0;

  // Print out information about this option. The to-be-maintained width is
  // specified.
  //
  virtual void printOptionInfo(size_t GlobalWidth) const = 0;

  virtual void printOptionValue(size_t GlobalWidth, bool Force) const = 0;

  virtual void setDefault() = 0;

  // Prints the help string for an option.
  static void printHelpStr(std::string_view HelpStr, size_t Indent,
                           size_t FirstLineIndentedBy);

  // Prints the help string for an enum value.
  static void printEnumValHelpStr(std::string_view HelpStr, size_t Indent,
                                  size_t FirstLineIndentedBy);

  virtual void getExtraOptionNames(std::vector<std::string_view> &) {}

  /// Get possible values for this option (for shell completion).
  /// For enum-style options, returns the valid value names.
  virtual void
  getOptionCompletionValues(std::vector<std::string_view> &) const {}

  // Wrapper around handleOccurrence that enforces Flags.
  //
  virtual bool addOccurrence(unsigned pos, std::string_view ArgName,
                             std::string_view Value, bool MultiArg = false);

  // Prints option name followed by message.  Always returns true.
  bool error(const std::string &Message, std::string_view ArgName = {},
             std::ostream &Errs = llcl::errs());
  bool error(const std::string &Message, std::ostream &Errs) {
    return error(Message, std::string_view{}, Errs);
  }

  inline int getNumOccurrences() const { return NumOccurrences; }
  void reset();
};

//===----------------------------------------------------------------------===//
// Command line option modifiers that can be used to modify the behavior of
// command line option parsers...
//

// Modifier to set the description shown in the -help output...
struct desc {
  std::string_view Desc;

  desc(std::string_view Str) : Desc(Str) {}

  void apply(Option &O) const { O.setDescription(Desc); }
};

// Modifier to set the value description shown in the -help output...
struct value_desc {
  std::string_view Desc;

  value_desc(std::string_view Str) : Desc(Str) {}

  void apply(Option &O) const { O.setValueStr(Desc); }
};

// Specify a default (initial) value for the command line argument.
template <class Ty> struct initializer {
  const Ty &Init;
  initializer(const Ty &Val) : Init(Val) {}

  template <class Opt> void apply(Opt &O) const { O.setInitialValue(Init); }
};

template <class Ty> struct list_initializer {
  const std::vector<Ty> &Inits;
  list_initializer(const std::vector<Ty> &Vals) : Inits(Vals) {}

  template <class Opt> void apply(Opt &O) const { O.setInitialValues(Inits); }
};

template <class Ty> initializer<Ty> init(const Ty &Val) {
  return initializer<Ty>(Val);
}

template <class Ty>
list_initializer<Ty> list_init(const std::vector<Ty> &Vals) {
  return list_initializer<Ty>(Vals);
}

// Allow the user to specify which external variable they want to store the
// results of the command line argument processing into.
template <class Ty> struct LocationClass {
  Ty &Loc;

  LocationClass(Ty &L) : Loc(L) {}

  template <class Opt> void apply(Opt &O) const { O.setLocation(O, Loc); }
};

template <class Ty> LocationClass<Ty> location(Ty &L) {
  return LocationClass<Ty>(L);
}

// Specify the Option category for the command line argument to belong to.
struct cat {
  OptionCategory &Category;

  cat(OptionCategory &c) : Category(c) {}

  template <class Opt> void apply(Opt &O) const { O.addCategory(Category); }
};

// Specify the subcommand that this option belongs to.
struct sub {
  SubCommand *Sub = nullptr;
  SubCommandGroup *Group = nullptr;

  sub(SubCommand &S) : Sub(&S) {}
  sub(SubCommandGroup &G) : Group(&G) {}

  template <class Opt> void apply(Opt &O) const {
    if (Sub)
      O.addSubCommand(*Sub);
    else if (Group)
      for (SubCommand *SC : Group->getSubCommands())
        O.addSubCommand(*SC);
  }
};

// Specify a callback function to be called when an option is seen.
template <typename R, typename Ty> struct cb {
  std::function<R(Ty)> CB;

  cb(std::function<R(Ty)> CB) : CB(CB) {}

  template <typename Opt> void apply(Opt &O) const { O.setCallback(CB); }
};

namespace detail {
template <typename F>
struct callback_traits : public callback_traits<decltype(&F::operator())> {};

template <typename R, typename C, typename... Args>
struct callback_traits<R (C::*)(Args...) const> {
  using result_type = R;
  using arg_type = std::tuple_element_t<0, std::tuple<Args...>>;
  static_assert(sizeof...(Args) == 1,
                "callback function must have one and only one parameter");
  static_assert(std::is_same_v<result_type, void>,
                "callback return type must be void");
  static_assert(std::is_lvalue_reference_v<arg_type> &&
                    std::is_const_v<std::remove_reference_t<arg_type>>,
                "callback arg_type must be a const lvalue reference");
};
} // namespace detail

template <typename F>
cb<typename detail::callback_traits<F>::result_type,
   typename detail::callback_traits<F>::arg_type>
callback(F CB) {
  using result_type = typename detail::callback_traits<F>::result_type;
  using arg_type = typename detail::callback_traits<F>::arg_type;
  return cb<result_type, arg_type>(CB);
}

//===----------------------------------------------------------------------===//

// Support value comparison outside the template.
struct GenericOptionValue {
  virtual bool compare(const GenericOptionValue &V) const = 0;

protected:
  GenericOptionValue() = default;
  GenericOptionValue(const GenericOptionValue &) = default;
  GenericOptionValue &operator=(const GenericOptionValue &) = default;
  ~GenericOptionValue() = default;

private:
  virtual void anchor();
};

template <class DataType> struct OptionValue;

// The default value safely does nothing.
template <class DataType, bool isClass>
struct OptionValueBase : GenericOptionValue {
  using WrapperType = OptionValue<DataType>;

  bool hasValue() const { return false; }

  const DataType &getValue() const { llcl_unreachable("no default value"); }

  template <class DT> void setValue(const DT & /*V*/) {}

  bool compare(const DataType & /*V*/) const { return false; }

  bool compare(const GenericOptionValue & /*V*/) const override {
    return false;
  }

protected:
  ~OptionValueBase() = default;
};

// Simple copy of the option value.
template <class DataType> class OptionValueCopy : public GenericOptionValue {
  DataType Value;
  bool Valid = false;

protected:
  OptionValueCopy(const OptionValueCopy &) = default;
  OptionValueCopy &operator=(const OptionValueCopy &) = default;
  ~OptionValueCopy() = default;

public:
  OptionValueCopy() = default;

  bool hasValue() const { return Valid; }

  const DataType &getValue() const {
    assert(Valid && "invalid option value");
    return Value;
  }

  void setValue(const DataType &V) {
    Valid = true;
    Value = V;
  }

  bool compare(const DataType &V) const { return Valid && (Value == V); }

  bool compare(const GenericOptionValue &V) const override {
    const OptionValueCopy<DataType> &VC =
        static_cast<const OptionValueCopy<DataType> &>(V);
    if (!VC.hasValue())
      return false;
    return compare(VC.getValue());
  }
};

// Non-class option values.
template <class DataType>
struct OptionValueBase<DataType, false> : OptionValueCopy<DataType> {
  using WrapperType = DataType;

protected:
  OptionValueBase() = default;
  OptionValueBase(const OptionValueBase &) = default;
  OptionValueBase &operator=(const OptionValueBase &) = default;
  ~OptionValueBase() = default;
};

// Top-level option class.
template <class DataType>
struct OptionValue final
    : OptionValueBase<DataType, std::is_class_v<DataType>> {
  OptionValue() = default;

  OptionValue(const DataType &V) { this->setValue(V); }

  template <class DT> OptionValue<DataType> &operator=(const DT &V) {
    this->setValue(V);
    return *this;
  }
};

// Other safe-to-copy-by-value common option types.
enum boolOrDefault { BOU_UNSET, BOU_TRUE, BOU_FALSE };
template <>
struct OptionValue<boolOrDefault> final : OptionValueCopy<boolOrDefault> {
  using WrapperType = boolOrDefault;

  OptionValue() = default;

  OptionValue(const boolOrDefault &V) { this->setValue(V); }

  OptionValue<boolOrDefault> &operator=(const boolOrDefault &V) {
    setValue(V);
    return *this;
  }

private:
  void anchor() override;
};

template <>
struct OptionValue<std::string> final : OptionValueCopy<std::string> {
  using WrapperType = std::string_view;

  OptionValue() = default;

  OptionValue(const std::string &V) { this->setValue(V); }

  OptionValue<std::string> &operator=(const std::string &V) {
    setValue(V);
    return *this;
  }

private:
  void anchor() override;
};

//===----------------------------------------------------------------------===//
// Enum valued command line option
//

// This represents a single enum value, using "int" as the underlying type.
struct OptionEnumValue {
  std::string_view Name;
  int Value;
  std::string_view Description;
};

#define clEnumVal(ENUMVAL, DESC)                                               \
  llcl::OptionEnumValue { #ENUMVAL, int(ENUMVAL), DESC }
#define clEnumValN(ENUMVAL, FLAGNAME, DESC)                                    \
  llcl::OptionEnumValue { FLAGNAME, int(ENUMVAL), DESC }

// For custom data types, allow specifying a group of values together.
class ValuesClass {
  std::vector<OptionEnumValue> Values;

public:
  ValuesClass(std::initializer_list<OptionEnumValue> Options)
      : Values(Options) {}

  template <class Opt> void apply(Opt &O) const {
    for (const auto &Value : Values)
      O.getParser().addLiteralOption(Value.Name, Value.Value,
                                     Value.Description);
  }
};

/// Helper to build a ValuesClass by forwarding a variable number of arguments.
template <typename... OptsTy> ValuesClass values(OptsTy... Options) {
  return ValuesClass({Options...});
}

//===----------------------------------------------------------------------===//
// Parameterizable parser for different data types.

//--------------------------------------------------
// This class holds all the non-generic code.
//
class generic_parser_base {
protected:
  class GenericOptionInfo {
  public:
    GenericOptionInfo(std::string_view name, std::string_view helpStr)
        : Name(name), HelpStr(helpStr) {}
    std::string_view Name;
    std::string_view HelpStr;
  };

public:
  generic_parser_base(Option &O) : Owner(O) {}

  virtual ~generic_parser_base() = default;

  virtual unsigned getNumOptions() const = 0;

  virtual std::string_view getOption(unsigned N) const = 0;

  virtual std::string_view getDescription(unsigned N) const = 0;

  virtual size_t getOptionWidth(const Option &O) const;

  virtual const GenericOptionValue &getOptionValue(unsigned N) const = 0;

  virtual void printOptionInfo(const Option &O, size_t GlobalWidth) const;

  void printGenericOptionDiff(const Option &O, const GenericOptionValue &V,
                              const GenericOptionValue &Default,
                              size_t GlobalWidth) const;

  template <class AnyOptionValue>
  void printOptionDiff(const Option &O, const AnyOptionValue &V,
                       const AnyOptionValue &Default,
                       size_t GlobalWidth) const {
    printGenericOptionDiff(O, V, Default, GlobalWidth);
  }

  void initialize() {}

  void getExtraOptionNames(std::vector<std::string_view> &OptionNames) {
    if (!Owner.hasArgStr())
      for (unsigned i = 0, e = getNumOptions(); i != e; ++i)
        OptionNames.push_back(getOption(i));
  }

  enum ValueExpected getValueExpectedFlagDefault() const {
    if (Owner.hasArgStr())
      return ValueRequired;
    else
      return ValueDisallowed;
  }

  unsigned findOption(std::string_view Name);

protected:
  Option &Owner;
};

// Default parser implementation.
template <class DataType> class parser : public generic_parser_base {
protected:
  class OptionInfo : public GenericOptionInfo {
  public:
    OptionInfo(std::string_view name, DataType v, std::string_view helpStr)
        : GenericOptionInfo(name, helpStr), V(v) {}

    OptionValue<DataType> V;
  };
  std::vector<OptionInfo> Values;

public:
  parser(Option &O) : generic_parser_base(O) {}

  using parser_data_type = DataType;

  unsigned getNumOptions() const override { return unsigned(Values.size()); }
  std::string_view getOption(unsigned N) const override {
    return Values[N].Name;
  }
  std::string_view getDescription(unsigned N) const override {
    return Values[N].HelpStr;
  }

  const GenericOptionValue &getOptionValue(unsigned N) const override {
    return Values[N].V;
  }

  // Return true on error.
  bool parse(Option &O, std::string_view ArgName, std::string_view Arg,
             DataType &V) {
    std::string_view ArgVal;
    if (Owner.hasArgStr())
      ArgVal = Arg;
    else
      ArgVal = ArgName;

    for (size_t i = 0, e = Values.size(); i != e; ++i)
      if (Values[i].Name == ArgVal) {
        V = Values[i].V.getValue();
        return false;
      }

    return O.error("Cannot find option named '" + std::string(ArgVal) + "'!");
  }

  /// Add an entry to the mapping table.
  template <class DT>
  void addLiteralOption(std::string_view Name, const DT &V,
                        std::string_view HelpStr) {
#ifndef NDEBUG
    if (findOption(Name) != Values.size())
      report_fatal_error("Option '" + std::string(Name) + "' already exists!");
#endif
    OptionInfo X(Name, static_cast<DataType>(V), HelpStr);
    Values.push_back(X);
    AddLiteralOption(Owner, Name);
  }

  /// Remove the specified option.
  void removeLiteralOption(std::string_view Name) {
    unsigned N = findOption(Name);
    assert(N != Values.size() && "Option not found!");
    Values.erase(Values.begin() + N);
  }
};

//--------------------------------------------------
// Super class of parsers to provide boilerplate code
//
class basic_parser_impl { // non-template implementation of basic_parser<t>
public:
  basic_parser_impl(Option &) {}

  virtual ~basic_parser_impl() = default;

  enum ValueExpected getValueExpectedFlagDefault() const {
    return ValueRequired;
  }

  void getExtraOptionNames(std::vector<std::string_view> &) {}

  void initialize() {}

  size_t getOptionWidth(const Option &O) const;

  void printOptionInfo(const Option &O, size_t GlobalWidth) const;

  void printOptionNoValue(const Option &O, size_t GlobalWidth) const;

  virtual std::string_view getValueName() const { return "value"; }

  virtual void anchor();

protected:
  void printOptionName(const Option &O, size_t GlobalWidth) const;
};

// The real basic parser is just a template wrapper.
template <class DataType> class basic_parser : public basic_parser_impl {
public:
  using parser_data_type = DataType;
  using OptVal = OptionValue<DataType>;

  basic_parser(Option &O) : basic_parser_impl(O) {}
};

//--------------------------------------------------

template <> class parser<bool> : public basic_parser<bool> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &O, std::string_view ArgName, std::string_view Arg,
             bool &Val);

  void initialize() {}

  enum ValueExpected getValueExpectedFlagDefault() const {
    return ValueOptional;
  }

  std::string_view getValueName() const override { return {}; }

  void printOptionDiff(const Option &O, bool V, OptVal Default,
                       size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <> class parser<boolOrDefault> : public basic_parser<boolOrDefault> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &O, std::string_view ArgName, std::string_view Arg,
             boolOrDefault &Val);

  enum ValueExpected getValueExpectedFlagDefault() const {
    return ValueOptional;
  }

  std::string_view getValueName() const override { return {}; }

  void printOptionDiff(const Option &O, boolOrDefault V, OptVal Default,
                       size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <> class parser<int> : public basic_parser<int> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &O, std::string_view ArgName, std::string_view Arg,
             int &Val);

  std::string_view getValueName() const override { return "int"; }

  void printOptionDiff(const Option &O, int V, OptVal Default,
                       size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <> class parser<long> final : public basic_parser<long> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &O, std::string_view ArgName, std::string_view Arg,
             long &Val);

  std::string_view getValueName() const override { return "long"; }

  void printOptionDiff(const Option &O, long V, OptVal Default,
                       size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <> class parser<long long> : public basic_parser<long long> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &O, std::string_view ArgName, std::string_view Arg,
             long long &Val);

  std::string_view getValueName() const override { return "long"; }

  void printOptionDiff(const Option &O, long long V, OptVal Default,
                       size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <> class parser<unsigned> : public basic_parser<unsigned> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &O, std::string_view ArgName, std::string_view Arg,
             unsigned &Val);

  std::string_view getValueName() const override { return "uint"; }

  void printOptionDiff(const Option &O, unsigned V, OptVal Default,
                       size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <>
class parser<unsigned long> final : public basic_parser<unsigned long> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &O, std::string_view ArgName, std::string_view Arg,
             unsigned long &Val);

  std::string_view getValueName() const override { return "ulong"; }

  void printOptionDiff(const Option &O, unsigned long V, OptVal Default,
                       size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <>
class parser<unsigned long long> : public basic_parser<unsigned long long> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &O, std::string_view ArgName, std::string_view Arg,
             unsigned long long &Val);

  std::string_view getValueName() const override { return "ulong"; }

  void printOptionDiff(const Option &O, unsigned long long V, OptVal Default,
                       size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <> class parser<double> : public basic_parser<double> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &O, std::string_view ArgName, std::string_view Arg,
             double &Val);

  std::string_view getValueName() const override { return "number"; }

  void printOptionDiff(const Option &O, double V, OptVal Default,
                       size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <> class parser<float> : public basic_parser<float> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &O, std::string_view ArgName, std::string_view Arg,
             float &Val);

  std::string_view getValueName() const override { return "number"; }

  void printOptionDiff(const Option &O, float V, OptVal Default,
                       size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <> class parser<std::string> : public basic_parser<std::string> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &, std::string_view, std::string_view Arg,
             std::string &Value) {
    Value = std::string(Arg);
    return false;
  }

  std::string_view getValueName() const override { return "string"; }

  void printOptionDiff(const Option &O, std::string_view V,
                       const OptVal &Default, size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <>
class parser<std::optional<std::string>>
    : public basic_parser<std::optional<std::string>> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &, std::string_view, std::string_view Arg,
             std::optional<std::string> &Value) {
    Value = std::string(Arg);
    return false;
  }

  std::string_view getValueName() const override { return "optional string"; }

  void printOptionDiff(const Option &O, std::optional<std::string_view> V,
                       const OptVal &Default, size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------

template <> class parser<char> : public basic_parser<char> {
public:
  parser(Option &O) : basic_parser(O) {}

  bool parse(Option &, std::string_view, std::string_view Arg, char &Value) {
    Value = Arg[0];
    return false;
  }

  std::string_view getValueName() const override { return "char"; }

  void printOptionDiff(const Option &O, char V, OptVal Default,
                       size_t GlobalWidth) const;

  void anchor() override;
};

//--------------------------------------------------
// This collection of wrappers is the intermediary between class opt and class
// parser to handle all the template nastiness.

// This overloaded function is selected by the generic parser.
template <class ParserClass, class DT>
void printOptionDiff(const Option &O, const generic_parser_base &P, const DT &V,
                     const OptionValue<DT> &Default, size_t GlobalWidth) {
  OptionValue<DT> OV = V;
  P.printOptionDiff(O, OV, Default, GlobalWidth);
}

// This is instantiated for basic parsers when the parsed value has a different
// type than the option value.
template <class ParserDT, class ValDT> struct OptionDiffPrinter {
  void print(const Option &O, const parser<ParserDT> &P, const ValDT & /*V*/,
             const OptionValue<ValDT> & /*Default*/, size_t GlobalWidth) {
    P.printOptionNoValue(O, GlobalWidth);
  }
};

// This is instantiated for basic parsers when the parsed value has the same
// type as the option value.
template <class DT> struct OptionDiffPrinter<DT, DT> {
  void print(const Option &O, const parser<DT> &P, const DT &V,
             const OptionValue<DT> &Default, size_t GlobalWidth) {
    P.printOptionDiff(O, V, Default, GlobalWidth);
  }
};

// This overloaded function is selected by the basic parser.
template <class ParserClass, class ValDT>
void printOptionDiff(
    const Option &O,
    const basic_parser<typename ParserClass::parser_data_type> &P,
    const ValDT &V, const OptionValue<ValDT> &Default, size_t GlobalWidth) {

  OptionDiffPrinter<typename ParserClass::parser_data_type, ValDT> printer;
  printer.print(O, static_cast<const ParserClass &>(P), V, Default,
                GlobalWidth);
}

//===----------------------------------------------------------------------===//
// Applicator support
//
template <class Mod> struct applicator {
  template <class Opt> static void opt(const Mod &M, Opt &O) { M.apply(O); }
};

// Handle const char* as a special case...
template <unsigned n> struct applicator<char[n]> {
  template <class Opt> static void opt(std::string_view Str, Opt &O) {
    O.setArgStr(Str);
  }
};
template <unsigned n> struct applicator<const char[n]> {
  template <class Opt> static void opt(std::string_view Str, Opt &O) {
    O.setArgStr(Str);
  }
};
template <> struct applicator<std::string_view> {
  template <class Opt> static void opt(std::string_view Str, Opt &O) {
    O.setArgStr(Str);
  }
};
template <> struct applicator<std::string> {
  template <class Opt> static void opt(const std::string &Str, Opt &O) {
    O.setArgStr(Str);
  }
};

template <> struct applicator<NumOccurrencesFlag> {
  static void opt(NumOccurrencesFlag N, Option &O) {
    O.setNumOccurrencesFlag(N);
  }
};

template <> struct applicator<ValueExpected> {
  static void opt(ValueExpected VE, Option &O) { O.setValueExpectedFlag(VE); }
};

template <> struct applicator<OptionHidden> {
  static void opt(OptionHidden OH, Option &O) { O.setHiddenFlag(OH); }
};

template <> struct applicator<FormattingFlags> {
  static void opt(FormattingFlags FF, Option &O) { O.setFormattingFlag(FF); }
};

template <> struct applicator<MiscFlags> {
  static void opt(MiscFlags MF, Option &O) {
    assert((MF != Grouping || O.ArgStr.size() == 1) &&
           "Grouping can only apply to single character Options.");
    O.setMiscFlag(MF);
  }
};

// Apply modifiers to an option in a type safe way.
template <class Opt, class Mod, class... Mods>
void apply(Opt *O, const Mod &M, const Mods &...Ms) {
  applicator<Mod>::opt(M, *O);
  apply(O, Ms...);
}

template <class Opt, class Mod> void apply(Opt *O, const Mod &M) {
  applicator<Mod>::opt(M, *O);
}

//===----------------------------------------------------------------------===//
// Default storage class definition: external storage.
//
template <class DataType, bool ExternalStorage, bool isClass>
class opt_storage {
  DataType *Location = nullptr; // Where to store the object...
  OptionValue<DataType> Default;

  void check_location() const {
    assert(Location && "location(...) not specified for a command "
                       "line option with external storage, "
                       "or init specified before location()!!");
  }

public:
  opt_storage() = default;

  bool setLocation(Option &O, DataType &L) {
    if (Location)
      return O.error("location(x) specified more than once!");
    Location = &L;
    Default = L;
    return false;
  }

  template <class T> void setValue(const T &V, bool initial = false) {
    check_location();
    *Location = V;
    if (initial)
      Default = V;
  }

  DataType &getValue() {
    check_location();
    return *Location;
  }
  const DataType &getValue() const {
    check_location();
    return *Location;
  }

  operator DataType() const { return this->getValue(); }

  const OptionValue<DataType> &getDefault() const { return Default; }
};

// Define how to hold a class type object, such as a string.
template <class DataType>
class opt_storage<DataType, false, true> : public DataType {
public:
  OptionValue<DataType> Default;

  template <class T> void setValue(const T &V, bool initial = false) {
    DataType::operator=(V);
    if (initial)
      Default = V;
  }

  DataType &getValue() { return *this; }
  const DataType &getValue() const { return *this; }

  const OptionValue<DataType> &getDefault() const { return Default; }
};

// Define a partial specialization to handle things we cannot inherit from.
template <class DataType> class opt_storage<DataType, false, false> {
public:
  DataType Value;
  OptionValue<DataType> Default;

  opt_storage() : Value(DataType()), Default() {}

  template <class T> void setValue(const T &V, bool initial = false) {
    Value = V;
    if (initial)
      Default = V;
  }
  DataType &getValue() { return Value; }
  DataType getValue() const { return Value; }

  const OptionValue<DataType> &getDefault() const { return Default; }

  operator DataType() const { return getValue(); }

  // If the datatype is a pointer, support -> on it.
  DataType operator->() const { return Value; }
};

//===----------------------------------------------------------------------===//
// A scalar command line option.
//
template <class DataType, bool ExternalStorage = false,
          class ParserClass = parser<DataType>>
class opt
    : public Option,
      public opt_storage<DataType, ExternalStorage, std::is_class_v<DataType>> {
  ParserClass Parser;

  bool handleOccurrence(unsigned pos, std::string_view ArgName,
                        std::string_view Arg) override {
    typename ParserClass::parser_data_type Val =
        typename ParserClass::parser_data_type();
    if (Parser.parse(*this, ArgName, Arg, Val))
      return true; // Parse error!
    this->setValue(Val);
    this->setPosition(pos);
    if (Callback)
      Callback(Val);
    return false;
  }

  enum ValueExpected getValueExpectedFlagDefault() const override {
    return Parser.getValueExpectedFlagDefault();
  }

  void
  getExtraOptionNames(std::vector<std::string_view> &OptionNames) override {
    return Parser.getExtraOptionNames(OptionNames);
  }

  size_t getOptionWidth() const override {
    return Parser.getOptionWidth(*this);
  }

  void printOptionInfo(size_t GlobalWidth) const override {
    Parser.printOptionInfo(*this, GlobalWidth);
  }

  void printOptionValue(size_t GlobalWidth, bool Force) const override {
    if (Force || !this->getDefault().compare(this->getValue())) {
      printOptionDiff<ParserClass>(*this, Parser, this->getValue(),
                                   this->getDefault(), GlobalWidth);
    }
  }

  void setDefault() override {
    if constexpr (std::is_assignable_v<DataType &, DataType>) {
      const OptionValue<DataType> &V = this->getDefault();
      if (V.hasValue())
        this->setValue(V.getValue());
      else
        this->setValue(DataType());
    }
  }

  void done() {
    addArgument();
    Parser.initialize();
  }

public:
  opt(const opt &) = delete;
  opt &operator=(const opt &) = delete;

  void setInitialValue(const DataType &V) { this->setValue(V, true); }

  ParserClass &getParser() { return Parser; }

  void getOptionCompletionValues(
      std::vector<std::string_view> &Values) const override {
    if constexpr (std::is_base_of_v<generic_parser_base, ParserClass>) {
      for (unsigned i = 0, e = Parser.getNumOptions(); i != e; ++i)
        Values.push_back(Parser.getOption(i));
    }
  }

  template <class T> DataType &operator=(const T &Val) {
    this->setValue(Val);
    if (Callback)
      Callback(Val);
    return this->getValue();
  }

  template <class T> DataType &operator=(T &&Val) {
    this->getValue() = std::forward<T>(Val);
    if (Callback)
      Callback(this->getValue());
    return this->getValue();
  }

  template <class... Mods>
  explicit opt(const Mods &...Ms)
      : Option(llcl::Optional, NotHidden), Parser(*this) {
    apply(this, Ms...);
    done();
  }

  void setCallback(
      std::function<void(const typename ParserClass::parser_data_type &)> CB) {
    Callback = CB;
  }

  std::function<void(const typename ParserClass::parser_data_type &)> Callback;
};

//===----------------------------------------------------------------------===//
// Default storage class definition: external storage.
//
template <class DataType, class StorageClass> class list_storage {
  StorageClass *Location = nullptr; // Where to store the object...
  std::vector<OptionValue<DataType>> Default =
      std::vector<OptionValue<DataType>>();
  bool DefaultAssigned = false;

public:
  list_storage() = default;

  void clear() {}

  bool setLocation(Option &O, StorageClass &L) {
    if (Location)
      return O.error("location(x) specified more than once!");
    Location = &L;
    return false;
  }

  template <class T> void addValue(const T &V, bool initial = false) {
    assert(Location != nullptr && "location(...) not specified for a command "
                                  "line option with external storage!");
    Location->push_back(V);
    if (initial)
      Default.push_back(V);
  }

  const std::vector<OptionValue<DataType>> &getDefault() const {
    return Default;
  }

  void assignDefault() { DefaultAssigned = true; }
  void overwriteDefault() { DefaultAssigned = false; }
  bool isDefaultAssigned() { return DefaultAssigned; }
};

// Define how to hold a class type object, such as a string.
template <class DataType> class list_storage<DataType, bool> {
  std::vector<DataType> Storage;
  std::vector<OptionValue<DataType>> Default;
  bool DefaultAssigned = false;

public:
  using iterator = typename std::vector<DataType>::iterator;

  iterator begin() { return Storage.begin(); }
  iterator end() { return Storage.end(); }

  using const_iterator = typename std::vector<DataType>::const_iterator;

  const_iterator begin() const { return Storage.begin(); }
  const_iterator end() const { return Storage.end(); }

  using size_type = typename std::vector<DataType>::size_type;

  size_type size() const { return Storage.size(); }

  bool empty() const { return Storage.empty(); }

  void push_back(const DataType &value) { Storage.push_back(value); }
  void push_back(DataType &&value) { Storage.push_back(value); }

  using reference = typename std::vector<DataType>::reference;
  using const_reference = typename std::vector<DataType>::const_reference;

  reference operator[](size_type pos) { return Storage[pos]; }
  const_reference operator[](size_type pos) const { return Storage[pos]; }

  void clear() { Storage.clear(); }

  iterator erase(const_iterator pos) { return Storage.erase(pos); }
  iterator erase(const_iterator first, const_iterator last) {
    return Storage.erase(first, last);
  }

  iterator erase(iterator pos) { return Storage.erase(pos); }
  iterator erase(iterator first, iterator last) {
    return Storage.erase(first, last);
  }

  iterator insert(const_iterator pos, const DataType &value) {
    return Storage.insert(pos, value);
  }
  iterator insert(const_iterator pos, DataType &&value) {
    return Storage.insert(pos, value);
  }

  iterator insert(iterator pos, const DataType &value) {
    return Storage.insert(pos, value);
  }
  iterator insert(iterator pos, DataType &&value) {
    return Storage.insert(pos, value);
  }

  reference front() { return Storage.front(); }
  const_reference front() const { return Storage.front(); }

  operator std::vector<DataType> &() { return Storage; }
  operator const std::vector<DataType> &() const { return Storage; }
  std::vector<DataType> *operator&() { return &Storage; }
  const std::vector<DataType> *operator&() const { return &Storage; }

  template <class T> void addValue(const T &V, bool initial = false) {
    Storage.push_back(V);
    if (initial)
      Default.push_back(OptionValue<DataType>(V));
  }

  const std::vector<OptionValue<DataType>> &getDefault() const {
    return Default;
  }

  void assignDefault() { DefaultAssigned = true; }
  void overwriteDefault() { DefaultAssigned = false; }
  bool isDefaultAssigned() { return DefaultAssigned; }
};

//===----------------------------------------------------------------------===//
// A list of command line options.
//
template <class DataType, class StorageClass = bool,
          class ParserClass = parser<DataType>>
class list : public Option, public list_storage<DataType, StorageClass> {
  std::vector<unsigned> Positions;
  ParserClass Parser;

  enum ValueExpected getValueExpectedFlagDefault() const override {
    return Parser.getValueExpectedFlagDefault();
  }

  void
  getExtraOptionNames(std::vector<std::string_view> &OptionNames) override {
    return Parser.getExtraOptionNames(OptionNames);
  }

  bool handleOccurrence(unsigned pos, std::string_view ArgName,
                        std::string_view Arg) override {
    typename ParserClass::parser_data_type Val =
        typename ParserClass::parser_data_type();
    if (list_storage<DataType, StorageClass>::isDefaultAssigned()) {
      clear();
      list_storage<DataType, StorageClass>::overwriteDefault();
    }
    if (Parser.parse(*this, ArgName, Arg, Val))
      return true; // Parse Error!
    list_storage<DataType, StorageClass>::addValue(Val);
    setPosition(pos);
    Positions.push_back(pos);
    if (Callback)
      Callback(Val);
    return false;
  }

  size_t getOptionWidth() const override {
    return Parser.getOptionWidth(*this);
  }

  void printOptionInfo(size_t GlobalWidth) const override {
    Parser.printOptionInfo(*this, GlobalWidth);
  }

  void printOptionValue(size_t /*GlobalWidth*/, bool /*Force*/) const override {
  }

  void setDefault() override {
    Positions.clear();
    list_storage<DataType, StorageClass>::clear();
    for (auto &Val : list_storage<DataType, StorageClass>::getDefault())
      list_storage<DataType, StorageClass>::addValue(Val.getValue());
  }

  void done() {
    addArgument();
    Parser.initialize();
  }

public:
  list(const list &) = delete;
  list &operator=(const list &) = delete;

  ParserClass &getParser() { return Parser; }

  void getOptionCompletionValues(
      std::vector<std::string_view> &Values) const override {
    if constexpr (std::is_base_of_v<generic_parser_base, ParserClass>) {
      for (unsigned i = 0, e = Parser.getNumOptions(); i != e; ++i)
        Values.push_back(Parser.getOption(i));
    }
  }

  unsigned getPosition(unsigned optnum) const {
    assert(optnum < this->size() && "Invalid option index");
    return Positions[optnum];
  }

  void clear() {
    Positions.clear();
    list_storage<DataType, StorageClass>::clear();
  }

  void setInitialValues(const std::vector<DataType> &Vs) {
    assert(!(list_storage<DataType, StorageClass>::isDefaultAssigned()) &&
           "Cannot have two default values");
    list_storage<DataType, StorageClass>::assignDefault();
    for (auto &Val : Vs)
      list_storage<DataType, StorageClass>::addValue(Val, true);
  }

  void setNumAdditionalVals(unsigned n) { Option::setNumAdditionalVals(n); }

  template <class... Mods>
  explicit list(const Mods &...Ms)
      : Option(ZeroOrMore, NotHidden), Parser(*this) {
    apply(this, Ms...);
    done();
  }

  void setCallback(
      std::function<void(const typename ParserClass::parser_data_type &)> CB) {
    Callback = CB;
  }

  std::function<void(const typename ParserClass::parser_data_type &)> Callback;
};

// Modifier to set the number of additional values.
struct multi_val {
  unsigned AdditionalVals;
  explicit multi_val(unsigned N) : AdditionalVals(N) {}

  template <typename D, typename S, typename P>
  void apply(list<D, S, P> &L) const {
    L.setNumAdditionalVals(AdditionalVals);
  }
};

//===----------------------------------------------------------------------===//
// bits_storage
//
template <class DataType, class StorageClass> class bits_storage {
  unsigned *Location = nullptr; // Where to store the bits...

  template <class T> static unsigned Bit(const T &V) {
    unsigned BitPos = static_cast<unsigned>(V);
    assert(BitPos < sizeof(unsigned) * CHAR_BIT &&
           "enum exceeds width of bit vector!");
    return 1 << BitPos;
  }

public:
  bits_storage() = default;

  bool setLocation(Option &O, unsigned &L) {
    if (Location)
      return O.error("location(x) specified more than once!");
    Location = &L;
    return false;
  }

  template <class T> void addValue(const T &V) {
    assert(Location != nullptr && "location(...) not specified for a command "
                                  "line option with external storage!");
    *Location |= Bit(V);
  }

  unsigned getBits() { return *Location; }

  void clear() {
    if (Location)
      *Location = 0;
  }

  template <class T> bool isSet(const T &V) {
    return (*Location & Bit(V)) != 0;
  }
};

template <class DataType> class bits_storage<DataType, bool> {
  unsigned Bits{0};

  template <class T> static unsigned Bit(const T &V) {
    unsigned BitPos = static_cast<unsigned>(V);
    assert(BitPos < sizeof(unsigned) * CHAR_BIT &&
           "enum exceeds width of bit vector!");
    return 1 << BitPos;
  }

public:
  template <class T> void addValue(const T &V) { Bits |= Bit(V); }

  unsigned getBits() { return Bits; }

  void clear() { Bits = 0; }

  template <class T> bool isSet(const T &V) { return (Bits & Bit(V)) != 0; }
};

//===----------------------------------------------------------------------===//
// A bit vector of command options.
//
template <class DataType, class Storage = bool,
          class ParserClass = parser<DataType>>
class bits : public Option, public bits_storage<DataType, Storage> {
  std::vector<unsigned> Positions;
  ParserClass Parser;

  enum ValueExpected getValueExpectedFlagDefault() const override {
    return Parser.getValueExpectedFlagDefault();
  }

  void
  getExtraOptionNames(std::vector<std::string_view> &OptionNames) override {
    return Parser.getExtraOptionNames(OptionNames);
  }

  bool handleOccurrence(unsigned pos, std::string_view ArgName,
                        std::string_view Arg) override {
    typename ParserClass::parser_data_type Val =
        typename ParserClass::parser_data_type();
    if (Parser.parse(*this, ArgName, Arg, Val))
      return true; // Parse Error!
    this->addValue(Val);
    setPosition(pos);
    Positions.push_back(pos);
    if (Callback)
      Callback(Val);
    return false;
  }

  size_t getOptionWidth() const override {
    return Parser.getOptionWidth(*this);
  }

  void printOptionInfo(size_t GlobalWidth) const override {
    Parser.printOptionInfo(*this, GlobalWidth);
  }

  void printOptionValue(size_t /*GlobalWidth*/, bool /*Force*/) const override {
  }

  void setDefault() override { bits_storage<DataType, Storage>::clear(); }

  void done() {
    addArgument();
    Parser.initialize();
  }

public:
  bits(const bits &) = delete;
  bits &operator=(const bits &) = delete;

  ParserClass &getParser() { return Parser; }

  void getOptionCompletionValues(
      std::vector<std::string_view> &Values) const override {
    if constexpr (std::is_base_of_v<generic_parser_base, ParserClass>) {
      for (unsigned i = 0, e = Parser.getNumOptions(); i != e; ++i)
        Values.push_back(Parser.getOption(i));
    }
  }

  unsigned getPosition(unsigned optnum) const {
    assert(optnum < this->size() && "Invalid option index");
    return Positions[optnum];
  }

  template <class... Mods>
  explicit bits(const Mods &...Ms)
      : Option(ZeroOrMore, NotHidden), Parser(*this) {
    apply(this, Ms...);
    done();
  }

  void setCallback(
      std::function<void(const typename ParserClass::parser_data_type &)> CB) {
    Callback = CB;
  }

  std::function<void(const typename ParserClass::parser_data_type &)> Callback;
};

//===----------------------------------------------------------------------===//
// Aliased command line option (alias this name to a preexisting name)
//

class alias : public Option {
  Option *AliasFor;

  bool handleOccurrence(unsigned pos, std::string_view /*ArgName*/,
                        std::string_view Arg) override {
    return AliasFor->handleOccurrence(pos, AliasFor->ArgStr, Arg);
  }

  bool addOccurrence(unsigned pos, std::string_view /*ArgName*/,
                     std::string_view Value, bool MultiArg = false) override {
    return AliasFor->addOccurrence(pos, AliasFor->ArgStr, Value, MultiArg);
  }

  size_t getOptionWidth() const override;
  void printOptionInfo(size_t GlobalWidth) const override;

  void printOptionValue(size_t /*GlobalWidth*/, bool /*Force*/) const override {
  }

  void setDefault() override { AliasFor->setDefault(); }

  ValueExpected getValueExpectedFlagDefault() const override {
    return AliasFor->getValueExpectedFlag();
  }

  void done() {
    if (!hasArgStr())
      error("alias must have argument name specified!");
    if (!AliasFor)
      error("alias must have an aliasopt(option) specified!");
    if (!Subs.empty())
      error("alias must not have sub(), aliased option's sub() "
            "will be used!");
    Subs = AliasFor->Subs;
    Categories = AliasFor->Categories;
    addArgument();
  }

public:
  alias(const alias &) = delete;
  alias &operator=(const alias &) = delete;

  void setAliasFor(Option &O) {
    if (AliasFor)
      error("alias must only have one aliasopt(...) specified!");
    AliasFor = &O;
  }

  template <class... Mods>
  explicit alias(const Mods &...Ms)
      : Option(Optional, Hidden), AliasFor(nullptr) {
    apply(this, Ms...);
    done();
  }
};

// Modifier to set the option an alias aliases.
struct aliasopt {
  Option &Opt;

  explicit aliasopt(Option &O) : Opt(O) {}

  void apply(alias &A) const { A.setAliasFor(Opt); }
};

// Provide additional help at the end of the normal help output.
struct extrahelp {
  std::string_view morehelp;

  explicit extrahelp(std::string_view help);
};

void PrintVersionMessage();

/// This function just prints the help message.
void PrintHelpMessage(bool Hidden = false, bool Categorized = false);

/// Print shell completion script to stdout.
/// Supported shells: "bash", "zsh".
void PrintShellCompletion(std::string_view Shell);

//===----------------------------------------------------------------------===//
// Public interface for accessing registered options.
//

/// Use this to get a map of all registered named options.
std::map<std::string, Option *, std::less<>> &
getRegisteredOptions(SubCommand &Sub = SubCommand::getTopLevel());

/// Use this to get all registered SubCommands from the provided parser.
std::unordered_set<SubCommand *> &getRegisteredSubcommands();

//===----------------------------------------------------------------------===//
// Standalone command line processing utilities.
//

/// Tokenizes a command line that can contain escapes and quotes.
void TokenizeGNUCommandLine(std::string_view Source, StringSaver &Saver,
                            std::vector<const char *> &NewArgv,
                            bool MarkEOLs = false);

/// Tokenizes a string of Windows command line arguments.
void TokenizeWindowsCommandLine(std::string_view Source, StringSaver &Saver,
                                std::vector<const char *> &NewArgv,
                                bool MarkEOLs = false);

/// Tokenizes a Windows command line while attempting to avoid copies.
void TokenizeWindowsCommandLineNoCopy(std::string_view Source,
                                      StringSaver &Saver,
                                      std::vector<std::string_view> &NewArgv);

/// Tokenizes a Windows full command line, including command name at the start.
void TokenizeWindowsCommandLineFull(std::string_view Source, StringSaver &Saver,
                                    std::vector<const char *> &NewArgv,
                                    bool MarkEOLs = false);

/// String tokenization function type.
using TokenizerCallback = void (*)(std::string_view Source, StringSaver &Saver,
                                   std::vector<const char *> &NewArgv,
                                   bool MarkEOLs);

/// Tokenizes content of configuration file.
void tokenizeConfigFile(std::string_view Source, StringSaver &Saver,
                        std::vector<const char *> &NewArgv,
                        bool MarkEOLs = false);

/// Contains options that control response file expansion.
class ExpansionContext {
  /// Provides persistent storage for parsed strings.
  StringSaver Saver;

  /// Tokenization strategy. Typically Unix or Windows.
  TokenizerCallback Tokenizer;

  /// Path used to resolve relative rsp files. If empty, the filesystem
  /// current directory is used instead.
  std::string_view CurrentDir;

  /// Directories used for search of config files.
  std::vector<std::string_view> SearchDirs;

  /// True if names of nested response files must be resolved relative to
  /// including file.
  bool RelativeNames = false;

  /// If true, mark end of lines and the end of the response file with nullptrs.
  bool MarkEOLs = false;

  /// If true, body of config file is expanded.
  bool InConfigFile = false;

  llcl::Error expandResponseFile(std::string_view FName,
                                 std::vector<const char *> &NewArgv);

public:
  ExpansionContext(BumpPtrAllocator &A, TokenizerCallback T);

  ExpansionContext &setMarkEOLs(bool X) {
    MarkEOLs = X;
    return *this;
  }

  ExpansionContext &setRelativeNames(bool X) {
    RelativeNames = X;
    return *this;
  }

  ExpansionContext &setCurrentDir(std::string_view X) {
    CurrentDir = X;
    return *this;
  }

  ExpansionContext &setSearchDirs(const std::vector<std::string_view> &X) {
    SearchDirs = X;
    return *this;
  }

  /// Looks for the specified configuration file.
  bool findConfigFile(std::string_view FileName, std::string &FilePath);

  /// Reads command line options from the given configuration file.
  llcl::Error readConfigFile(std::string_view CfgFile,
                             std::vector<const char *> &Argv);

  /// Expands constructs "@file" in the provided array of arguments recursively.
  llcl::Error expandResponseFiles(std::vector<const char *> &Argv);
};

/// A convenience helper which concatenates the options specified by the
/// environment variable EnvVar and command line options, then expands
/// response files recursively.
bool expandResponseFiles(int Argc, const char *const *Argv, const char *EnvVar,
                         std::vector<const char *> &NewArgv);

/// A convenience helper which supports the typical use case of expansion.
bool ExpandResponseFiles(StringSaver &Saver, TokenizerCallback Tokenizer,
                         std::vector<const char *> &Argv);

/// A convenience helper which concatenates env var and command line options.
bool expandResponseFiles(int Argc, const char *const *Argv, const char *EnvVar,
                         StringSaver &Saver,
                         std::vector<const char *> &NewArgv);

/// Mark all options not part of this category as ReallyHidden.
void HideUnrelatedOptions(OptionCategory &Category,
                          SubCommand &Sub = SubCommand::getTopLevel());

/// Mark all options not part of the categories as ReallyHidden.
void HideUnrelatedOptions(const std::vector<const OptionCategory *> &Categories,
                          SubCommand &Sub = SubCommand::getTopLevel());

/// Reset all command line options to a state that looks as if they have
/// never appeared on the command line.
void ResetAllOptionOccurrences();

/// Reset the command line parser back to its initial state.
void ResetCommandLineParser();

/// Parses `Arg` into the option handler `Handler`.
bool ProvidePositionalOption(Option *Handler, std::string_view Arg, int i);

} // end namespace llcl

#endif // LLCL_SUPPORT_COMMANDLINE_H
