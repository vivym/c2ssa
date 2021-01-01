#include "PrintingPass.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PatternMatch.h"
#include "llvm/IR/PrintPasses.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/TargetRegistry.h"
#include "IDMap.h"
#include "CBackend.h"
#include <set>

using namespace llvm;
using namespace c2ssa;

PrintFunctionPass::PrintFunctionPass(raw_ostream &OS, CWriter &writer, const std::string &Banner,
                                     bool ShouldPreserveUseListOrder)
    : OS(OS), writer(writer), Banner(Banner),
      ShouldPreserveUseListOrder(ShouldPreserveUseListOrder) {}

PreservedAnalyses PrintFunctionPass::run(Function &F, FunctionAnalysisManager &AM) {
  if (!Banner.empty())
    OS << Banner << "\n";

  auto &LI = AM.getResult<LoopAnalysis>(F);
  auto TD = new DataLayout(F.getParent());
  // CWriter writer(OS, F, &LI, TD);
  writer.printFunction(F, &LI);

  return PreservedAnalyses::all();
}

PrintGlobalsPass::PrintGlobalsPass(raw_ostream &OS, CWriter &writer, const std::string &Banner,
                                     bool ShouldPreserveUseListOrder)
    : OS(OS), writer(writer), Banner(Banner),
      ShouldPreserveUseListOrder(ShouldPreserveUseListOrder) {}

PreservedAnalyses PrintGlobalsPass::run(Module &M, ModuleAnalysisManager &AM) {
  if (!Banner.empty())
    OS << Banner << "\n";

  return PreservedAnalyses::all();
}
