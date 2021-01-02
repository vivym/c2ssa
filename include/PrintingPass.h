#ifndef C2SSA_PRINTINGPASS_H
#define C2SSA_PRINTINGPASS_H

#include "llvm/IR/PassManager.h"
#include "CBackend.h"
#include <string>

namespace llvm {
  class raw_ostream;
}

namespace c2ssa {

class PrintFunctionPass : public llvm::PassInfoMixin<PrintFunctionPass> {
  CWriter &writer;

public:
  PrintFunctionPass(CWriter &writer);

  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &AM);
  static bool isRequired() { return true; }
};

class PrintGlobalsPass : public llvm::PassInfoMixin<PrintGlobalsPass> {
  CWriter &writer;

public:
  PrintGlobalsPass(CWriter &writer);

  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &AM);
  static bool isRequired() { return true; }
};

class PrintModulePass : public llvm::PassInfoMixin<PrintGlobalsPass> {
  llvm::raw_ostream &OS;
  std::string Banner;
  
public:
  PrintModulePass();
  PrintModulePass(llvm::raw_ostream &OS, const std::string &Banner = "");
  
  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &AM);
  static bool isRequired() { return true; }
};

FunctionPass *createPrintModulePass(raw_ostream &OS,
                                  const std::string &Banner = "");

void initializeC2SSAPrintModulePass(PassRegistry &Registry);

} // namespace c2ssa

#endif
