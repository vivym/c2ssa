#ifndef CODEGEN_CODEGENACTION_H
#define CODEGEN_CODEGENACTION_H

#include "clang/Frontend/FrontendAction.h"
#include <memory>

namespace llvm {
  class LLVMContext;
  class Module;
}

namespace c2ssa {
class BackendConsumer;

class CodeGenAction : public clang::ASTFrontendAction {
private:
  llvm::LLVMContext *VMContext;
  bool OwnsVMContext;

public:
  /// Create a new code generation action.  If the optional \p _VMContext
  /// parameter is supplied, the action uses it without taking ownership,
  /// otherwise it creates a fresh LLVM context and takes ownership.
  CodeGenAction(llvm::LLVMContext *_VMContext = nullptr);

  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(clang::CompilerInstance &CI,
                                                        clang::StringRef InFile) override;

public:
  ~CodeGenAction() override;

  BackendConsumer *BEConsumer;
};

}

#endif
