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
