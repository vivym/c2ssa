// Declares clang::SyntaxOnlyAction.
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Analysis/CFG.h"
#include "clang/CodeGen/CodeGenAction.h"
// Declares llvm::cl::extrahelp.
#include "llvm/Support/CommandLine.h"
#include "CodeGenAction.h"
#include "llvm/ADT/SmallVector.h"

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory C11SSACategory("c2ssa options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static llvm::cl::extrahelp CommonHelp(clang::tooling::CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static llvm::cl::extrahelp MoreHelp("\nMore help text...\n");

class MyCallback : public clang::ast_matchers::MatchFinder::MatchCallback {
 public:
  MyCallback(){}
  void run(const clang::ast_matchers::MatchFinder::MatchResult &Result){
    const auto* Function = Result.Nodes.getNodeAs<clang::FunctionDecl>("fn");
    const auto CFG = clang::CFG::buildCFG(Function,
                                        Function->getBody(),
                                        Result.Context,
                                        clang::CFG::BuildOptions());
    for (const auto* blk : *CFG){
      blk->dump();        // Prints Basic Blocks.
      // do something more.
    }
  }
};

class MyConsumer : public clang::ASTConsumer {
public:
  explicit MyConsumer() : handler() {
    const auto matching_node = clang::ast_matchers::functionDecl(clang::ast_matchers::isExpansionInMainFile()).bind("fn");
    match_finder.addMatcher(matching_node, &handler);
  }
  void HandleTranslationUnit(clang::ASTContext& ctx) {
    match_finder.matchAST(ctx);
  }  
private:  
  MyCallback handler;  
  clang::ast_matchers::MatchFinder match_finder;  
};

class TestAction : public clang::ASTFrontendAction {
public:
    std::unique_ptr<clang::ASTConsumer>
    CreateASTConsumer(clang::CompilerInstance&, llvm::StringRef) override {
        return std::make_unique<MyConsumer>();
    }
};

int main(int argc, const char **argv) {
    clang::tooling::CommonOptionsParser OptionsParser(argc, argv, C11SSACategory);
    clang::tooling::ClangTool Tool(
        OptionsParser.getCompilations(),
        OptionsParser.getSourcePathList()
    );
    
    llvm::SmallVector<const char *, 16> BackendArgs;
    BackendArgs.push_back("clang"); // Fake program name.
    llvm::cl::ParseCommandLineOptions(BackendArgs.size() - 1,
                                      BackendArgs.data());
    return Tool.run(clang::tooling::newFrontendActionFactory<c2ssa::CodeGenAction>().get());
    // return Tool.run(clang::tooling::newFrontendActionFactory<TestAction>().get());
}
