// Declares clang::SyntaxOnlyAction.
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Analysis/CFG.h"
#include "clang/CodeGen/CodeGenAction.h"
// Declares llvm::cl::extrahelp.
#include "llvm/Support/CommandLine.h"
#include "CodeGenAction.h"
#include <vector>

/*
// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory C11SSACategory("c2ssa options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static llvm::cl::extrahelp CommonHelp(clang::tooling::CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static llvm::cl::extrahelp MoreHelp("\nMore help text...\n");
*/

int main(int argc, const char **argv) {
  // clang::tooling::CommonOptionsParser OptionsParser(argc, argv, C11SSACategory);
  std::vector<const char *> CommandLine;
  std::vector<std::string> StrippedArgs;
  std::unique_ptr<clang::tooling::CompilationDatabase> Compilations = std::make_unique<clang::tooling::FixedCompilationDatabase>(".", StrippedArgs);
  if (argc != 2) {
      return -1;
  }
  std::vector<std::string> SourcePathList(argv + 1, argv + 2);
  clang::tooling::ClangTool Tool(
      *Compilations,
      SourcePathList
  );

  return Tool.run(clang::tooling::newFrontendActionFactory<c2ssa::CodeGenAction>().get());
}
