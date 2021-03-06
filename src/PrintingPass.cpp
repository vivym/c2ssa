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

namespace c2ssa {

PrintFunctionPass::PrintFunctionPass(CWriter &writer)
    : writer(writer) {}

PreservedAnalyses PrintFunctionPass::run(Function &F, FunctionAnalysisManager &AM) {
  auto &LI = AM.getResult<LoopAnalysis>(F);
  writer.printFunction(F, &LI);

  return PreservedAnalyses::all();
}

PrintGlobalsPass::PrintGlobalsPass(CWriter &writer)
    : writer(writer) {}

PreservedAnalyses PrintGlobalsPass::run(Module &M, ModuleAnalysisManager &AM) {

  return PreservedAnalyses::all();
}

PrintModulePass::PrintModulePass() : OS(dbgs()), Banner("") {}
PrintModulePass::PrintModulePass(raw_ostream &OS, const std::string &Banner)
    : OS(OS), Banner(Banner) {}

PreservedAnalyses PrintModulePass::run(Module &M, ModuleAnalysisManager &AM) {
  std::string OutStr;
  std::string OutHeadersStr;
  raw_string_ostream Out(OutStr);
  raw_string_ostream OutHeaders(OutHeadersStr);
  
  if (!Banner.empty())
    OS << Banner << "\n";
  
  FunctionAnalysisManager &FAM =
      AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
 
  // Request PassInstrumentation from analysis manager, will use it to run
  // instrumenting callbacks for the passes later.
  PassInstrumentation PI = AM.getResult<PassInstrumentationAnalysis>(M);

  const llvm::DataLayout *TD = new DataLayout(&M);
  llvm::IntrinsicLowering *IL = new IntrinsicLowering(*TD);
  CWriter writer(Out, OutHeaders, &M, TD, IL);
  
  using PassModelT =
      detail::PassModel<Function, PrintFunctionPass, PreservedAnalyses,
                        FunctionAnalysisManager>;
  auto FunctionPass = std::make_unique<PassModelT>(PrintFunctionPass(writer));
  
  PreservedAnalyses PA = PreservedAnalyses::all();
  for (Function &F : M) {
    if (F.isDeclaration())
      continue;
    
    // Check the PassInstrumentation's BeforePass callbacks before running the
    // pass, skip its execution completely if asked to (callback returns
    // false).
    if (!PI.runBeforePass<Function>(*FunctionPass, F))
      continue;
    
    PreservedAnalyses PassPA;
    {
      TimeTraceScope TimeScope(FunctionPass->name(), F.getName());
      PassPA = FunctionPass->run(F, FAM);
    }
    
    PI.runAfterPass(*FunctionPass, F, PassPA);
    
    // We know that the function pass couldn't have invalidated any other
    // function's analyses (that's the contract of a function pass), so
    // directly handle the function analysis manager's invalidation here.
    FAM.invalidate(F, PassPA);

    // Then intersect the preserved set so that invalidation of module
    // analyses will eventually occur when the module pass completes.
    PA.intersect(std::move(PassPA));
    
    auto &LI = FAM.getResult<LoopAnalysis>(F);
  }

  // The FunctionAnalysisManagerModuleProxy is preserved because (we assume)
  // the function passes we ran didn't add or remove any functions.
  //
  // We also preserve all analyses on Functions, because we did all the
  // invalidation we needed to do above.
  PA.preserveSet<AllAnalysesOn<Function>>();
  PA.preserve<FunctionAnalysisManagerModuleProxy>();
  
  std::string methods = Out.str();
  OutStr.clear();
  writer.generateHeader(M);
  std::string headers = OutHeaders.str() + Out.str();
  OutStr.clear();
  OutHeadersStr.clear();
  OS << headers << methods;
  
  delete TD;
  delete IL;
  
  return PA;
}

namespace {

class C2SSAPrintModulePass : public FunctionPass {
  CWriter *writer = nullptr;
  raw_ostream &OS;
  const std::string Banner;
  std::string OutStr;
  std::string OutHeadersStr;
  raw_string_ostream Out;
  raw_string_ostream OutHeaders;
  
  llvm::DataLayout *TD = nullptr;
  llvm::IntrinsicLowering *IL = nullptr;
  
public:
  static char ID;
  C2SSAPrintModulePass() : FunctionPass(ID), OS(dbgs()), Banner(""), OutStr(""), OutHeadersStr(""), Out(OutStr), OutHeaders(OutHeadersStr) {}
  C2SSAPrintModulePass(raw_ostream &OS, const std::string &Banner)
      : FunctionPass(ID), OS(OS), Banner(Banner), OutStr(""), OutHeadersStr(""), Out(OutStr), OutHeaders(OutHeadersStr) {}
  
  bool doInitialization(Module &M) override {
    TD = new DataLayout(&M);
    IL = new IntrinsicLowering(*TD);
    writer = new CWriter(Out, OutHeaders, &M, TD, IL);
    
    return false;
  }
  
  bool doFinalization(Module &M) override {
    std::string methods = Out.str();
    OutStr.clear();
    writer->generateHeader(M);
    std::string headers = OutHeaders.str() + Out.str();
    OutStr.clear();
    OutHeadersStr.clear();
    OS << headers << methods;
    
    delete TD;
    delete IL;
    
    return true;
  }
  
  bool runOnFunction(Function &F) override {
    if (F.hasAvailableExternallyLinkage())
      return false;
    
    auto LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    writer->printFloatingPointConstants(F);
    writer->printFunction(F, LI);
    
    return false;
  }
  
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<LoopInfoWrapperPass>();
    AU.setPreservesAll();
  }
  
  StringRef getPassName() const override { return "C2SSA Print Function IR"; }
};

}

char C2SSAPrintModulePass::ID = 0;
static void *initializeC2SSAPrintModulePassOnce(PassRegistry &Registry) {
  llvm::initializeLoopInfoWrapperPassPass(Registry);
  
  PassInfo *PI = new PassInfo(
      "Print module to stderr", "c2ssa-print-module", &C2SSAPrintModulePass::ID,
      PassInfo::NormalCtor_t(callDefaultCtor<C2SSAPrintModulePass>), true, true);
  Registry.registerPass(*PI, true);
  return PI;
}
static llvm::once_flag InitializeC2SSAPrintModulePassFlag;
void initializeC2SSAPrintModulePass(PassRegistry &Registry) {
  llvm::call_once(InitializeC2SSAPrintModulePassFlag,
                  initializeC2SSAPrintModulePassOnce, std::ref(Registry));
}

FunctionPass *createPrintModulePass(llvm::raw_ostream &OS,
                                  const std::string &Banner) {
  // return new C2SSAPrintModulePassWrapper(OS, Banner);
  return new C2SSAPrintModulePass(OS, Banner);
}

} // namespace c2ssa
