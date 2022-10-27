#include <tuple>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/JsonSupport.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Sema/ParsedAttr.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaDiagnostic.h"
#include "llvm/IR/Attributes.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;

namespace {

struct DocDescription {
  StringRef text;
  SourceRange text_range;
  Decl *decl;
  Scope *scope;
};
std::vector<DocDescription> DocDescriptions;

struct DocUsage {
  StringRef text;
  SourceRange text_range;
};
std::vector<DocUsage> DocUsages;

std::string OutFile;

SourceRange subRange(SourceLocation whole_location, StringRef whole_text,
                     StringRef sub_text) {
  assert(sub_text.data() >= whole_text.data());
  assert(sub_text.data() + sub_text.size() <=
         whole_text.data() + whole_text.size());

  auto begin =
      whole_location.getLocWithOffset(sub_text.data() - whole_text.data());
  auto end = begin.getLocWithOffset(sub_text.size());
  return {begin, end};
}

IdentifierInfo *getIdentifier(ASTContext &context, StringRef name,
                              bool create_if_not_found = false) {
  if (create_if_not_found) {
    return &context.Idents.get(name);
  }
  if (auto it = context.Idents.find(name); it != context.Idents.end()) {
    return it->second;
  }
  return nullptr;
};

bool isValidFileRange(const SourceManager &source, SourceRange range) {
  if (!range.getBegin().isValid() || !range.getEnd().isValid())
    return false;

  FileID BeginFID;
  size_t BeginOffset = 0;
  std::tie(BeginFID, BeginOffset) = source.getDecomposedLoc(range.getBegin());

  FileID EndFID;
  size_t EndOffset = 0;
  std::tie(EndFID, EndOffset) = source.getDecomposedLoc(range.getEnd());

  return BeginFID.isValid() && BeginFID == EndFID && BeginOffset <= EndOffset;
}

StringRef toSourceCode(const SourceManager &source, SourceRange range) {
  assert(isValidFileRange(source, range));
  auto Buf = source.getBufferOrNone(source.getFileID(range.getBegin()));
  assert(Buf);

  size_t BeginOffset = source.getFileOffset(range.getBegin());
  // SourceRange is inclusive of the last char
  size_t EndOffset = 1 + source.getFileOffset(range.getEnd());
  return Buf->getBuffer().substr(BeginOffset, EndOffset - BeginOffset);
}

void findReferences(SmallVectorImpl<StringRef> &out, StringRef text) {
  SmallVector<StringRef, 3> parts;

  while (true) {
    text.split(parts, '`', /*MaxSplit=*/2);
    if (parts.size() != 3) {
      break;
    }

    (void)parts[0];          // before the first backtick
    out.push_back(parts[1]); // between the first and second backticks
    text = parts[2];         // after the second backtick
    parts.clear();
  }
}

// Try to look up the NamedDecl corresponding a potentially qualified name
// in a provided scope. If lookup fails nullptr will be returned.
// TODO there's probably a utility for doing this already and I just haven't
// found it...
// TODO handle template instantiations like `std::vector<int>::iterator`
NamedDecl *lookupName(StringRef name, Sema &sema, Scope *scope) {
  SmallVector<StringRef, 4> names;
  name.split(names, "::");
  assert(!names.empty());

  ASTContext &context = sema.getASTContext();

  if (names[0] == "") {
    // name is absolutely qualified, look it up at translation unit scope
    return lookupName(name.split("::").second, sema, sema.TUScope);
  }

  if (names.size() == 1) {
    // name is not qualified, just look it up directly
    if (auto *identifier = getIdentifier(context, name)) {
      return sema.LookupSingleName(scope, {identifier}, SourceLocation(),
                                   Sema::LookupAnyName);
    }
    return nullptr;
  }

  // name is qualified, now we need to look up its scope.
  NamedDecl *decl = lookupName(names[0], sema, scope);
  names.erase(names.begin());
  if (!decl) {
    return nullptr;
  }

  while (!names.empty()) {
    const DeclContext *scope_decl_context = dyn_cast<DeclContext>(decl);
    if (!scope_decl_context) {
      return nullptr;
    }

    auto *identifier = getIdentifier(context, names[0]);
    names.erase(names.begin());
    if (!identifier) {
      return nullptr;
    }

    auto result = scope_decl_context->lookup({identifier});
    if (!result.isSingleResult()) {
      return nullptr;
    }

    decl = result.front();
  }

  return decl;
}

template <typename Printables>
void printJoined(llvm::raw_ostream &ros, llvm::StringRef separator,
                 const Printables &printables) {
  llvm::ListSeparator sep{separator};
  for (const auto &printable : printables) {
    ros << sep << printable;
  }
}

template <typename Printable>
void printJoined(llvm::raw_ostream &ros, llvm::StringRef separator,
                 std::initializer_list<Printable> printables) {
  llvm::ListSeparator sep{separator};
  for (const auto &printable : printables) {
    ros << sep << printable;
  }
}

void printEscapedAndQuoted(llvm::raw_ostream &ros, llvm::StringRef string) {
  // llvm::printEscapedString prints newline as \0A, which isn't valid JSON
  ros << '"';
  for (char c : string) {
    switch (c) {
    default:
      ros << c;
      continue;
    case '\n':
      ros << "\\n";
      continue;
    case '\t':
      ros << "\\t";
      continue;
    case '\\':
      ros << "\\\\";
      continue;
    case '"':
      ros << "\\\"";
      continue;
    }
  }
  ros << '"';
}

void printSourceRangeAsJson(llvm::raw_ostream &ros, clang::SourceRange range,
                            const clang::SourceManager &source_manager) {
  ros << "[";
  printSourceLocationAsJson(ros, range.getBegin(), source_manager);
  ros << ", ";
  printSourceLocationAsJson(ros, range.getEnd(), source_manager);
  ros << "]";
}

void printDeclarationAsJson(llvm::raw_ostream &ros, const Decl *decl,
                            clang::SourceManager &source_manager) {
  ros << "{";

  printEscapedAndQuoted(ros, "name");
  ros << ": ";
  if (const NamedDecl *named_decl = dyn_cast<NamedDecl>(decl)) {
    printEscapedAndQuoted(ros, named_decl->getQualifiedNameAsString());
  } else {
    ros << "null";
  }
  ros << ", ";

  printEscapedAndQuoted(ros, "location");
  ros << ": ";
  printSourceLocationAsJson(ros, decl->getLocation(), source_manager);
  ros << "}";
}

void printDocDescriptionAsJson(llvm::raw_ostream &ros,
                               const DocDescription &doc_description,
                               clang::Sema &sema) {
  clang::SourceManager &source_manager = sema.getSourceManager();

  auto [text, text_range, decl, scope] = doc_description;

  ros << "{\n";

  printEscapedAndQuoted(ros, "type");
  ros << ": ";
  printEscapedAndQuoted(ros, "description");
  ros << ",\n";

  printEscapedAndQuoted(ros, "declaration");
  ros << ": ";
  printDeclarationAsJson(ros, decl, source_manager);
  ros << ",\n";

  printEscapedAndQuoted(ros, "text");
  ros << ": ";
  printEscapedAndQuoted(ros, text);
  ros << ",\n";

  printEscapedAndQuoted(ros, "text_range");
  ros << ": ";
  printSourceRangeAsJson(ros, text_range, source_manager);
  ros << ",\n";

  printEscapedAndQuoted(ros, "references");
  ros << ": ";
  ros << "[\n";

  SmallVector<StringRef, 32> referenced_names;
  findReferences(referenced_names, text);

  llvm::ListSeparator sep{", "};
  for (StringRef referenced_name : referenced_names) {
    ros << sep << "{\n";

    printEscapedAndQuoted(ros, "text");
    ros << ": ";
    printEscapedAndQuoted(ros, referenced_name);
    ros << ",\n";

    printEscapedAndQuoted(ros, "location");
    ros << ": ";
    printSourceRangeAsJson(
        ros, subRange(text_range.getBegin(), text, referenced_name),
        source_manager);
    ros << ",\n";

    printEscapedAndQuoted(ros, "referenced_declaration");
    ros << ": ";
    if (const NamedDecl *referenced =
            lookupName(referenced_name, sema, scope)) {
      printDeclarationAsJson(ros, referenced, source_manager);
    } else {
      ros << "null";
    }
    ros << "\n}";
  }
  ros << "]";
  ros << "\n}";
}

void printDocUsageAsJson(llvm::raw_ostream &ros, const DocUsage &doc_usage,
                         clang::Sema &sema) {
  clang::SourceManager &source_manager = sema.getSourceManager();

  auto [text, text_range] = doc_usage;

  ros << "{\n";

  printEscapedAndQuoted(ros, "type");
  ros << ": ";
  printEscapedAndQuoted(ros, "usage");
  ros << ",\n";

  printEscapedAndQuoted(ros, "text");
  ros << ": ";
  printEscapedAndQuoted(ros, text);
  ros << ",\n";

  printEscapedAndQuoted(ros, "text_range");
  ros << ": ";
  printSourceRangeAsJson(ros, text_range, source_manager);
  ros << ",\n";
}

class DocDescriptionAttrInfo final : public ParsedAttrInfo {
public:
  DocDescriptionAttrInfo() {
    // Spelled with c++11 style:
    // [[doc::description(R"(Explanation of the decl)")]]
    // except BUG: https://github.com/llvm/llvm-project/issues/46197
    // ... means that for now we accept GNU style as well, since c++11 style
    // fails to parse arguments when declared by a plugin.
    static constexpr Spelling kSpellings[] = {
        {ParsedAttr::AS_CXX11, "doc::description"},
        {ParsedAttr::AS_GNU, "description"},
    };
    Spellings = kSpellings;

    // Takes exactly one argument: a raw string literal containing
    // the description of the decorated Decl.
    NumArgs = 1;
    static constexpr const char *kArgNames[] = {"description_text"};
    ArgNames = kArgNames;

    AttrKind = AttributeCommonInfo::NoSemaHandlerAttribute;
  }

  bool diagAppertainsToDecl(Sema &, const ParsedAttr &,
                            const Decl *) const override {
    // any decl may be documented
    return true;
  }

  AttrHandling handleDeclAttribute(Sema &sema, Decl *decl,
                                   const ParsedAttr &attr) const override {
    assert(attr.getNumArgs() == 1);

    Scope *scope = sema.getScopeForContext(decl->getDeclContext());
    assert(scope != nullptr && "Scope for DeclContext was unknown");

    auto *argument =
        dyn_cast<StringLiteral>(attr.getArgAsExpr(0)->IgnoreParenCasts());

    SourceLocation text_location = argument->getExprLoc();
    StringRef text = argument->getString();

    auto attr_source = toSourceCode(sema.getSourceManager(), attr.getRange());
    size_t i = attr_source.find(text);
    if (i == StringRef::npos) {
      unsigned ID = sema.getDiagnostics().getCustomDiagID(
          DiagnosticsEngine::Error,
          "doc::description requires a string literal with no escapes (try a "
          "raw string literal)");
      sema.Diag(argument->getBeginLoc(), ID);
      // TODO .AddFixItHint(FixItHint::CreateReplacement(CharRange,
      //                                                 TextAsRawString));
      return AttributeNotApplied;
    }

    auto text_begin = attr.getRange().getBegin().getLocWithOffset(i);
    auto text_end = text_begin.getLocWithOffset(text.size());

    DocDescriptions.push_back({
        .text = text,
        .text_range = {text_begin, text_end},
        .decl = decl,
        .scope = scope,
    });
    return AttributeApplied;
  }
};

class DocUsageAttrInfo final : public ParsedAttrInfo {
public:
  DocUsageAttrInfo() {
    static constexpr Spelling kSpellings[] = {
        {ParsedAttr::AS_CXX11, "doc::usage"},
        {ParsedAttr::AS_GNU, "usage"},
    };
    Spellings = kSpellings;

    // Takes exactly one argument: a lambda illustrating usage
    // of the Decl.
    NumArgs = 1;
    static constexpr const char *kArgNames[] = {"usage_lambda"};
    ArgNames = kArgNames;

    AttrKind = AttributeCommonInfo::NoSemaHandlerAttribute;
  }

  bool diagAppertainsToDecl(Sema &, const ParsedAttr &,
                            const Decl *) const override {
    // any decl may be documented
    return true;
  }

  AttrHandling handleDeclAttribute(Sema &sema, Decl *decl,
                                   const ParsedAttr &attr) const override {
    ASTContext &context = sema.getASTContext();
    assert(attr.getNumArgs() == 1);

    LambdaExpr *usage_lambda =
        dyn_cast<LambdaExpr>(attr.getArgAsExpr(0)->IgnoreParenCasts());

    if (auto *op = usage_lambda->getCallOperator();
        op->getNumParams() != 0 or
        op->getReturnType()->getLocallyUnqualifiedSingleStepDesugaredType() !=
            context.VoidTy) {
      unsigned ID = sema.getDiagnostics().getCustomDiagID(
          DiagnosticsEngine::Error,
          "doc::usage requires a lambda with no parameters which returns void");
      sema.Diag(usage_lambda->getBeginLoc(), ID);
      return AttributeNotApplied;
    }

    CXXMethodDecl *static_invoker =
        usage_lambda->getLambdaClass()->getLambdaStaticInvoker();
    assert(static_invoker && static_invoker->isStatic());

    // For now, we only check whether usage lambdas compile and store their
    // source for inclusion in API doc. Eventually, it'd be nice to also run
    // them to be sure they don't segfault, trigger asserts, etc.
    //
    //
    // The simplest way to do this is to dump the usage sources into a
    // usages.gen.cc file which can be compiled and run for this purpose.
    //
    // That seems like a waste, though (not to mention fragile!)- we have
    // already compiled the lambda after all. It seems like we could avoid
    // recompilation by mutating the AST slightly: just add a function which is
    // full of CallExprs to the lambda's static invoker.
    //
    //     import ctypes
    //     lib = ctypes.cdll.LoadLibrary('./a.out')
    //     lib.__DocAttr_usages() # run all the usages
    //
    // However, I haven't managed to mutate the AST at all. Added statements
    // appear in dump() but not in the generated object file. Oh well?
    //
    //
    // As long as we're doing source gen, maybe the better approach is to
    // simplify by removing doc::usage altogether and extracting usages from ```
    // blocks in a doc::description string. Then we'd check that they compile
    // by assembling and compiling usages.gen.cc
    //
    // In this case we lose the advantages of syntax highlighting, language
    // server integration, and immediate breakage feedback which are conferred
    // by keeping usages as first class expressions. On the other hand those
    // things require that developers compile with clang and configure it to use
    // this plugin, which is perhaps unreasonable to expect anyway.

    SourceRange text_range = usage_lambda->getBody()->getSourceRange();
    StringRef text = toSourceCode(sema.getSourceManager(), text_range);
    DocUsages.push_back({.text = text, .text_range = text_range});

    // TODO it'd be quite useful and relatively easy to build a list of links
    // for anything referenced in the usage lambda; in generated doc this could
    // allow mouseover of `auto` to reveal the resolved type.

    return AttributeApplied;
  }
};

class ExtractDocAttrsConsumer : public ASTConsumer {
  CompilerInstance &compiler;
  std::string input_file_path;

public:
  ExtractDocAttrsConsumer(CompilerInstance &compiler, StringRef input_file_path)
      : compiler(compiler), input_file_path(input_file_path) {}

  void HandleTranslationUnit(ASTContext &context) override {
    clang::Sema &sema = compiler.getSema();

    auto status = llvm::writeToOutput(OutFile, [&](llvm::raw_ostream &ros) {
      ros << "[\n";
      llvm::ListSeparator sep{",\n"};

      for (const auto &doc_description : DocDescriptions) {
        ros << sep;
        printDocDescriptionAsJson(ros, doc_description, sema);
      }

      for (const auto &doc_usage : DocUsages) {
        ros << sep;
        printDocUsageAsJson(ros, doc_usage, sema);
      }

      ros << "\n]";
      return llvm::Error::success();
    });

    if (status) {
      llvm::errs() << status;
    }
  }
};

class ExtractDocAttrsAction : public PluginASTAction {
protected:
  std::unique_ptr<ASTConsumer>
  CreateASTConsumer(CompilerInstance &compiler,
                    StringRef input_file_path) override {
    return std::make_unique<ExtractDocAttrsConsumer>(compiler, input_file_path);
  }

  bool ParseArgs(const CompilerInstance &,
                 const std::vector<std::string> &args) override {
    if (args.empty() || args[0] == "help") {
      PrintHelp(llvm::errs());
      return false;
    }

    OutFile = args[0];
    return true;
  }

  void PrintHelp(llvm::raw_ostream &ros) {
    ros << R"(
      Usage:
      ------

      $ clang++ -cc1 -load libDocAttr.so          \
           -plugin extract_doc_attrs              \
           -plugin-arg-extract_doc_attrs doc.json \

      This will produce a JSON dump of all documentation
      attributes encountered during compilation.
    )";
  }
};

} // namespace

ParsedAttrInfoRegistry::Add<DocDescriptionAttrInfo>
    kDocDescriptionRegistrar("doc::description",
                             R"DOC(
The doc::description attribute can be used to specify a description
of the declaration which they decorate:

    [[doc::description("R(the classic greeting)")]]
    void greet(std::string_view target) {
      std::cout << "hello " << target << std::endl;
    }

The text of the description will be stored for later generation of documentation
along with the declaration's fully qualified name (if available) and precise location
in the source. References to other declarations may be made by bracketing a reference
with ``; the compiler will be used to look up the referenced entity.)DOC");

ParsedAttrInfoRegistry::Add<DocUsageAttrInfo> kDocUsageRegistrar("doc::usage",
                                                                 R"DOC(
  The doc::usage attribute can be used to illustrate usage of a declaration.
  The lambda will be checked for validity at compile time:

      [[doc::usage([] { greet("world"); })]]
      void greet(std::string_view);

      // Compilation error:
      // [[doc::usage([] { greet(0); })]]
      // void greet(std::string_view);
)DOC");

FrontendPluginRegistry::Add<ExtractDocAttrsAction>
    kActionRegistrar("extract_doc_attrs",
                     "Extract documentation attributes for analysis.");
