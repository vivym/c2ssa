#ifndef C2SSA_CALLGRAPH_H
#define C2SSA_CALLGRAPH_H
#include "CBackend.h"

namespace c2ssa {

class CallGraphCounter : public llvm::ModulePass {
public:
  static char ID;
  llvm::raw_ostream &Out;
  
  CallGraphCounter(): ModulePass(ID), Out(dbgs()) {}
  CallGraphCounter(llvm::raw_ostream &Out): ModulePass(ID), Out(Out) {}
  
  bool runOnModule(llvm::Module &M) {
    Out << "/** Call Graph\n";
    for (llvm::Function &F : M) {
      for (llvm::Function::iterator iter = F.begin(); iter != F.end(); ++iter) {
        for (llvm::BasicBlock::iterator Biter = iter->begin(); Biter != iter->end(); ++Biter) {
          llvm::Instruction *I = &*Biter;
          if (llvm::CallInst *inst = llvm::dyn_cast<llvm::CallInst>(I)) {
            llvm::Function* called = inst->getCalledFunction();
            Out << called->getName() << "\tcalled by\t";
            Out << F.getName() << "\n";
          }
        }
      }
    }
    
    Out << "**/\n\n";

    return false;
  }
};

} // namespace c2ssa

#endif
