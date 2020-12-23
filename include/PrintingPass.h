#ifndef PRINTINGPASS_H
#define PRINTINGPASS_H

#include "llvm/IR/PassManager.h"
#include <string>

namespace llvm {
  class raw_ostream;
}

namespace c2ssa {

class PrintModulePass : public llvm::PassInfoMixin<PrintModulePass> {
  llvm::raw_ostream &OS;
  std::string Banner;
  bool ShouldPreserveUseListOrder;

public:
  PrintModulePass();
  PrintModulePass(llvm::raw_ostream &OS, const std::string &Banner = "",
                  bool ShouldPreserveUseListOrder = false);

  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &AM);
  static bool isRequired() { return true; }
};

} // namespace c2ssa

#endif
