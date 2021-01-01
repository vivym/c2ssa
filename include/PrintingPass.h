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
  llvm::raw_ostream &OS;
  CWriter &writer;
  std::string Banner;
  bool ShouldPreserveUseListOrder;

public:
  PrintFunctionPass(llvm::raw_ostream &OS, CWriter &writer, const std::string &Banner = "",
                    bool ShouldPreserveUseListOrder = false);

  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &AM);
  static bool isRequired() { return true; }
};

class PrintGlobalsPass : public llvm::PassInfoMixin<PrintGlobalsPass> {
  llvm::raw_ostream &OS;
  CWriter &writer;
  std::string Banner;
  bool ShouldPreserveUseListOrder;

public:
  PrintGlobalsPass(llvm::raw_ostream &OS, CWriter &writer, const std::string &Banner = "",
                   bool ShouldPreserveUseListOrder = false);

  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &AM);
  static bool isRequired() { return true; }
};

} // namespace c2ssa

#endif
