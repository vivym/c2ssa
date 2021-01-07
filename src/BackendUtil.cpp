#include "BackendUtil.h"
#include "clang/Basic/CodeGenOptions.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Frontend/FrontendDiagnostic.h"
#include "clang/Frontend/Utils.h"
#include "clang/Lex/HeaderSearchOptions.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/CFLAndersAliasAnalysis.h"
#include "llvm/Analysis/CFLSteensAliasAnalysis.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/ScopedNoAliasAA.h"
#include "llvm/Analysis/TypeBasedAliasAnalysis.h"
#include "llvm/Analysis/StackSafetyAnalysis.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/CodeGen/RegAllocRegistry.h"
#include "llvm/CodeGen/SchedulerRegistry.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ModuleSummaryIndex.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/LTO/LTOBackend.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/BuryPointer.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TimeProfiler.h"
#include "llvm/Support/Timer.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/AggressiveInstCombine/AggressiveInstCombine.h"
#include "llvm/Transforms/Coroutines.h"
#include "llvm/Transforms/Coroutines/CoroCleanup.h"
#include "llvm/Transforms/Coroutines/CoroEarly.h"
#include "llvm/Transforms/Coroutines/CoroElide.h"
#include "llvm/Transforms/Coroutines/CoroSplit.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/IPO/Annotation2Metadata.h"
#include "llvm/Transforms/IPO/ArgumentPromotion.h"
#include "llvm/Transforms/IPO/Attributor.h"
#include "llvm/Transforms/IPO/BlockExtractor.h"
#include "llvm/Transforms/IPO/CalledValuePropagation.h"
#include "llvm/Transforms/IPO/ConstantMerge.h"
#include "llvm/Transforms/IPO/CrossDSOCFI.h"
#include "llvm/Transforms/IPO/DeadArgumentElimination.h"
#include "llvm/Transforms/IPO/ElimAvailExtern.h"
#include "llvm/Transforms/IPO/ForceFunctionAttrs.h"
#include "llvm/Transforms/IPO/FunctionAttrs.h"
#include "llvm/Transforms/IPO/FunctionImport.h"
#include "llvm/Transforms/IPO/GlobalDCE.h"
#include "llvm/Transforms/IPO/GlobalOpt.h"
#include "llvm/Transforms/IPO/GlobalSplit.h"
#include "llvm/Transforms/IPO/HotColdSplitting.h"
#include "llvm/Transforms/IPO/InferFunctionAttrs.h"
#include "llvm/Transforms/IPO/Inliner.h"
#include "llvm/Transforms/IPO/Internalize.h"
#include "llvm/Transforms/IPO/LoopExtractor.h"
#include "llvm/Transforms/IPO/LowerTypeTests.h"
#include "llvm/Transforms/IPO/MergeFunctions.h"
#include "llvm/Transforms/IPO/OpenMPOpt.h"
#include "llvm/Transforms/IPO/PartialInlining.h"
#include "llvm/Transforms/IPO/SCCP.h"
#include "llvm/Transforms/IPO/SampleProfile.h"
#include "llvm/Transforms/IPO/SampleProfileProbe.h"
#include "llvm/Transforms/IPO/StripDeadPrototypes.h"
#include "llvm/Transforms/IPO/StripSymbols.h"
#include "llvm/Transforms/IPO/SyntheticCountsPropagation.h"
#include "llvm/Transforms/IPO/WholeProgramDevirt.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Instrumentation.h"
#include "llvm/Transforms/Instrumentation/AddressSanitizer.h"
#include "llvm/Transforms/Instrumentation/BoundsChecking.h"
#include "llvm/Transforms/Instrumentation/CGProfile.h"
#include "llvm/Transforms/Instrumentation/DataFlowSanitizer.h"
#include "llvm/Transforms/Instrumentation/GCOVProfiler.h"
#include "llvm/Transforms/Instrumentation/HWAddressSanitizer.h"
#include "llvm/Transforms/Instrumentation/InstrProfiling.h"
#include "llvm/Transforms/Instrumentation/MemProfiler.h"
#include "llvm/Transforms/Instrumentation/MemorySanitizer.h"
#include "llvm/Transforms/Instrumentation/SanitizerCoverage.h"
#include "llvm/Transforms/Instrumentation/ThreadSanitizer.h"
#include "llvm/Transforms/ObjCARC.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/ADCE.h"
#include "llvm/Transforms/Scalar/AlignmentFromAssumptions.h"
#include "llvm/Transforms/Scalar/AnnotationRemarks.h"
#include "llvm/Transforms/Scalar/BDCE.h"
#include "llvm/Transforms/Scalar/CallSiteSplitting.h"
#include "llvm/Transforms/Scalar/ConstantHoisting.h"
#include "llvm/Transforms/Scalar/ConstraintElimination.h"
#include "llvm/Transforms/Scalar/CorrelatedValuePropagation.h"
#include "llvm/Transforms/Scalar/DCE.h"
#include "llvm/Transforms/Scalar/DeadStoreElimination.h"
#include "llvm/Transforms/Scalar/DivRemPairs.h"
#include "llvm/Transforms/Scalar/EarlyCSE.h"
#include "llvm/Transforms/Scalar/Float2Int.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/GuardWidening.h"
#include "llvm/Transforms/Scalar/IVUsersPrinter.h"
#include "llvm/Transforms/Scalar/IndVarSimplify.h"
#include "llvm/Transforms/Scalar/InductiveRangeCheckElimination.h"
#include "llvm/Transforms/Scalar/InstSimplifyPass.h"
#include "llvm/Transforms/Scalar/JumpThreading.h"
#include "llvm/Transforms/Scalar/LICM.h"
#include "llvm/Transforms/Scalar/LoopAccessAnalysisPrinter.h"
#include "llvm/Transforms/Scalar/LoopDataPrefetch.h"
#include "llvm/Transforms/Scalar/LoopDeletion.h"
#include "llvm/Transforms/Scalar/LoopDistribute.h"
#include "llvm/Transforms/Scalar/LoopFlatten.h"
#include "llvm/Transforms/Scalar/LoopFuse.h"
#include "llvm/Transforms/Scalar/LoopIdiomRecognize.h"
#include "llvm/Transforms/Scalar/LoopInstSimplify.h"
#include "llvm/Transforms/Scalar/LoopInterchange.h"
#include "llvm/Transforms/Scalar/LoopLoadElimination.h"
#include "llvm/Transforms/Scalar/LoopPassManager.h"
#include "llvm/Transforms/Scalar/LoopPredication.h"
#include "llvm/Transforms/Scalar/LoopReroll.h"
#include "llvm/Transforms/Scalar/LoopRotation.h"
#include "llvm/Transforms/Scalar/LoopSimplifyCFG.h"
#include "llvm/Transforms/Scalar/LoopSink.h"
#include "llvm/Transforms/Scalar/LoopStrengthReduce.h"
#include "llvm/Transforms/Scalar/LoopUnrollAndJamPass.h"
#include "llvm/Transforms/Scalar/LoopUnrollPass.h"
#include "llvm/Transforms/Scalar/LoopVersioningLICM.h"
#include "llvm/Transforms/Scalar/LowerAtomic.h"
#include "llvm/Transforms/Scalar/LowerConstantIntrinsics.h"
#include "llvm/Transforms/Scalar/LowerExpectIntrinsic.h"
#include "llvm/Transforms/Scalar/LowerGuardIntrinsic.h"
#include "llvm/Transforms/Scalar/LowerMatrixIntrinsics.h"
#include "llvm/Transforms/Scalar/LowerWidenableCondition.h"
#include "llvm/Transforms/Scalar/MakeGuardsExplicit.h"
#include "llvm/Transforms/Scalar/MemCpyOptimizer.h"
#include "llvm/Transforms/Scalar/MergeICmps.h"
#include "llvm/Transforms/Scalar/MergedLoadStoreMotion.h"
#include "llvm/Transforms/Scalar/NaryReassociate.h"
#include "llvm/Transforms/Scalar/NewGVN.h"
#include "llvm/Transforms/Scalar/PartiallyInlineLibCalls.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/Reg2Mem.h"
#include "llvm/Transforms/Scalar/RewriteStatepointsForGC.h"
#include "llvm/Transforms/Scalar/SCCP.h"
#include "llvm/Transforms/Scalar/ScalarizeMaskedMemIntrin.h"
#include "llvm/Transforms/Scalar/Scalarizer.h"
#include "llvm/Transforms/Scalar/SeparateConstOffsetFromGEP.h"
#include "llvm/Transforms/Scalar/SimpleLoopUnswitch.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "llvm/Transforms/Scalar/Sink.h"
#include "llvm/Transforms/Scalar/SpeculateAroundPHIs.h"
#include "llvm/Transforms/Scalar/SpeculativeExecution.h"
#include "llvm/Transforms/Scalar/StraightLineStrengthReduce.h"
#include "llvm/Transforms/Scalar/StructurizeCFG.h"
#include "llvm/Transforms/Scalar/TailRecursionElimination.h"
#include "llvm/Transforms/Scalar/WarnMissedTransforms.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/AddDiscriminators.h"
#include "llvm/Transforms/Utils/AssumeBundleBuilder.h"
#include "llvm/Transforms/Utils/BreakCriticalEdges.h"
#include "llvm/Transforms/Utils/CanonicalizeAliases.h"
#include "llvm/Transforms/Utils/CanonicalizeFreezeInLoops.h"
#include "llvm/Transforms/Utils/EntryExitInstrumenter.h"
#include "llvm/Transforms/Utils/FixIrreducible.h"
#include "llvm/Transforms/Utils/InjectTLIMappings.h"
#include "llvm/Transforms/Utils/InstructionNamer.h"
#include "llvm/Transforms/Utils/LCSSA.h"
#include "llvm/Transforms/Utils/LibCallsShrinkWrap.h"
#include "llvm/Transforms/Utils/LoopSimplify.h"
#include "llvm/Transforms/Utils/LoopVersioning.h"
#include "llvm/Transforms/Utils/LowerInvoke.h"
#include "llvm/Transforms/Utils/LowerSwitch.h"
#include "llvm/Transforms/Utils/Mem2Reg.h"
#include "llvm/Transforms/Utils/MetaRenamer.h"
#include "llvm/Transforms/Utils/NameAnonGlobals.h"
#include "llvm/Transforms/Utils/StripGCRelocates.h"
#include "llvm/Transforms/Utils/StripNonLineTableDebugInfo.h"
#include "llvm/Transforms/Utils/SymbolRewriter.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/Transforms/Utils/UnifyLoopExits.h"
#include "llvm/Transforms/Utils/UniqueInternalLinkageNames.h"
#include "llvm/Transforms/Vectorize/LoadStoreVectorizer.h"
#include "llvm/Transforms/Vectorize/LoopVectorize.h"
#include "llvm/Transforms/Vectorize/SLPVectorizer.h"
#include "llvm/Transforms/Vectorize/VectorCombine.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "PrintingPass.h"
#include "LoopUnrollPass.h"
#include "CallGraph.h"
#include "SROA.h"
#include <memory>
#include <sstream>
using namespace clang;
using namespace llvm;
using namespace c2ssa;

namespace {

class EmitAssemblyHelper {
  DiagnosticsEngine &Diags;
  const HeaderSearchOptions &HSOpts;
  const CodeGenOptions &CodeGenOpts;
  const clang::TargetOptions &TargetOpts;
  const LangOptions &LangOpts;
  Module *TheModule;

  Timer CodeGenerationTime;

  std::unique_ptr<raw_pwrite_stream> OS;

  TargetIRAnalysis getTargetIRAnalysis() const {
    return TargetIRAnalysis();
  }
  
  void CreatePasses(legacy::PassManager &MPM, legacy::FunctionPassManager &FPM);
  
  void addFunctionSimplificationPasses(legacy::PassManagerBase &MPM);

public:
  EmitAssemblyHelper(DiagnosticsEngine &_Diags,
                     const HeaderSearchOptions &HeaderSearchOpts,
                     const CodeGenOptions &CGOpts,
                     const clang::TargetOptions &TOpts,
                     const LangOptions &LOpts, Module *M)
      : Diags(_Diags), HSOpts(HeaderSearchOpts), CodeGenOpts(CGOpts),
        TargetOpts(TOpts), LangOpts(LOpts), TheModule(M),
        CodeGenerationTime("codegen", "Code Generation Time") {}

  void EmitAssembly(std::unique_ptr<raw_pwrite_stream> OS);
};

}

void EmitAssemblyHelper::addFunctionSimplificationPasses(legacy::PassManagerBase &MPM) {
  MPM.add(c2ssa::createSROAPass());
  MPM.add(createEarlyCSEPass(true /* Enable mem-ssa. */)); // Catch trivial redundancies

  MPM.add(createConstraintEliminationPass());

  // Speculative execution if the target has divergent branches; otherwise nop.
  MPM.add(createSpeculativeExecutionIfHasBranchDivergencePass());

  MPM.add(createJumpThreadingPass());         // Thread jumps.
  MPM.add(createCorrelatedValuePropagationPass()); // Propagate conditionals

  MPM.add(createCFGSimplificationPass());     // Merge & remove BBs
  // Combine silly seq's
  MPM.add(createAggressiveInstCombinerPass());
  MPM.add(createInstructionCombiningPass());
  MPM.add(createLibCallsShrinkWrapPass());
  
  // Optimize memory intrinsic calls based on the profiled size information.
  MPM.add(createPGOMemOPSizeOptLegacyPass());

  // TODO: Investigate the cost/benefit of tail call elimination on debugging.
  MPM.add(createTailCallEliminationPass());   // Eliminate tail calls
  MPM.add(createCFGSimplificationPass());     // Merge & remove BBs
  MPM.add(createReassociatePass());           // Reassociate expressions

  // Begin the loop pass pipeline.
  // Rotate Loop
  MPM.add(createLoopRotatePass(-1));
  MPM.add(createLoopUnswitchPass(false, false));
  // FIXME: We break the loop pass pipeline here in order to do full
  // simplify-cfg. Eventually loop-simplifycfg should be enhanced to replace the
  // need for this.
  MPM.add(createCFGSimplificationPass());
  MPM.add(createInstructionCombiningPass());
  // We resume loop passes creating a second loop pipeline here.
  MPM.add(createLoopFlattenPass()); // Flatten loops
  MPM.add(createLoopSimplifyCFGPass());
  MPM.add(createLoopIdiomPass());             // Recognize idioms like memset.
  MPM.add(createIndVarSimplifyPass());        // Canonicalize indvars
  MPM.add(createLoopDeletionPass());          // Delete dead loops

  MPM.add(createLoopInterchangePass()); // Interchange loops

  // Unroll small loops
  MPM.add(createSimpleLoopUnrollPass(CodeGenOpts.OptimizationLevel, false, false));
  // This ends the loop pass pipelines.

  // Break up allocas that may now be splittable after loop unrolling.
  MPM.add(c2ssa::createSROAPass());

  MPM.add(createMergedLoadStoreMotionPass()); // Merge ld/st in diamonds
  MPM.add(createGVNPass(false)); // Remove redundancies
  MPM.add(createMemCpyOptPass());             // Remove memcpy / form memset
  MPM.add(createSCCPPass());                  // Constant prop with SCCP

  MPM.add(createConstraintEliminationPass());

  // Delete dead bit computations (instcombine runs after to fold away the dead
  // computations, and then ADCE will run later to exploit any new DCE
  // opportunities that creates).
  MPM.add(createBitTrackingDCEPass());        // Delete dead bit computations

  // Run instcombine after redundancy elimination to exploit opportunities
  // opened up by them.
  MPM.add(createInstructionCombiningPass());
  MPM.add(createJumpThreadingPass());         // Thread jumps
  MPM.add(createCorrelatedValuePropagationPass());
  MPM.add(createAggressiveDCEPass()); // Delete dead instructions

  MPM.add(createDeadStoreEliminationPass());  // Delete dead stores

  MPM.add(createCFGSimplificationPass()); // Merge & remove BBs
  // Clean up after everything.
  MPM.add(createInstructionCombiningPass());
}

void EmitAssemblyHelper::CreatePasses(legacy::PassManager &MPM,
                                      legacy::FunctionPassManager &FPM) {
  Triple TargetTriple(TheModule->getTargetTriple());
  std::unique_ptr<TargetLibraryInfoImpl> TLII(new TargetLibraryInfoImpl(TargetTriple));

  MPM.add(new TargetLibraryInfoWrapperPass(*TLII));

  // Set up the per-function pass manager.
  FPM.add(new TargetLibraryInfoWrapperPass(*TLII));
  if (CodeGenOpts.VerifyModule)
    FPM.add(createVerifierPass());

  // build function pass
  {
    FPM.add(createTypeBasedAAWrapperPass());
    FPM.add(createScopedNoAliasAAWrapperPass());

    FPM.add(createCFGSimplificationPass());
    FPM.add(c2ssa::createSROAPass());
    // Common Subexpression Elimination
    FPM.add(createEarlyCSEPass());
  }
  
  {
    MPM.add(createTypeBasedAAWrapperPass());
    MPM.add(createScopedNoAliasAAWrapperPass());
    
    // Infer attributes about declarations if possible.
    MPM.add(createInferFunctionAttrsLegacyPass());
    
    // This pass propagates constants from call sites into the
    // bodies of functions, and keeps track of whether basic blocks are executable
    // in the process.
    MPM.add(createIPSCCPPass());
    // Attach metadata to indirct call sites
    // indicating the set of functions they may target at run-time.
    MPM.add(createCalledValuePropagationPass());

    MPM.add(createGlobalOptimizerPass()); // Optimize out global vars
    // Promote any localized global vars.
    // PromoteMemoryToRegister - This pass is used to promote memory references to
    // be register references. A simple example of the transformation performed by
    // this pass is:
    //
    //        FROM CODE                           TO CODE
    //   %X = alloca i32, i32 1                 ret i32 42
    //   store i32 42, i32 *%X
    //   %Y = load i32* %X
    //   ret i32 %Y
    //
    MPM.add(createPromoteMemoryToRegisterPass());

    MPM.add(createDeadArgEliminationPass()); // Dead argument elimination

    MPM.add(createInstructionCombiningPass()); // Clean up after IPCP & DAE
    // CFGSimplification - Merge basic blocks, eliminate unreachable blocks,
    // simplify terminator instructions, convert switches to lookup tables, etc.
    MPM.add(createCFGSimplificationPass()); // Clean up after IPCP & DAE

    // We add a module alias analysis pass here. In part due to bugs in the
    // analysis infrastructure this "works" in that the analysis stays alive
    // for the entire SCC pass run below.
    MPM.add(createGlobalsAAWrapperPass());

    // Start of CallGraph SCC passes.
    MPM.add(createPruneEHPass()); // Remove dead EH info

    MPM.add(createPostOrderFunctionAttrsLegacyPass());

    addFunctionSimplificationPasses(MPM);

    MPM.add(createReversePostOrderFunctionAttrsPass());

    // We add a fresh GlobalsModRef run at this point. This is particularly
    // useful as the above will have inlined, DCE'ed, and function-attr
    // propagated everything. We should at this point have a reasonably minimal
    // and richly annotated call graph. By computing aliasing and mod/ref
    // information for all local globals here, the late loop passes and notably
    // the vectorizer will be able to use them to help recognize vectorizable
    // memory operations.
    //
    // Note that this relies on a bug in the pass manager which preserves
    // a module analysis into a function pass pipeline (and throughout it) so
    // long as the first function pass doesn't invalidate the module analysis.
    // Thus both Float2Int and LoopRotate have to preserve AliasAnalysis for
    // this to work. Fortunately, it is trivial to preserve AliasAnalysis
    // (doing nothing preserves it as it is required to be conservatively
    // correct in the face of IR changes).
    MPM.add(createGlobalsAAWrapperPass());

    // Re-rotate loops in all our loop nests. These may have fallout out of
    // rotated form due to GVN or other transformations, and the vectorizer relies
    // on the rotated form. Disable header duplication at -Oz.
    MPM.add(createLoopRotatePass(-1));

    // Eliminate loads by forwarding stores from the previous iteration to loads
    // of the current iteration.
    MPM.add(createLoopLoadEliminationPass());

    MPM.add(createInstructionCombiningPass());

    // Cleanup after loop vectorization, etc. Simplification passes like CVP and
    // GVN, loop transforms, and others have already run, so it's now better to
    // convert to more optimized IR using more aggressive simplify CFG options.
    // The extra sinking transform can create larger basic blocks, so do this
    // before SLP vectorization.
    // FIXME: study whether hoisting and/or sinking of common instructions should
    //        be delayed until after SLP vectorizer.
    MPM.add(createCFGSimplificationPass(SimplifyCFGOptions()
                                            .forwardSwitchCondToPhi(true)
                                            .convertSwitchToLookupTable(true)
                                            .needCanonicalLoops(false)
                                            .hoistCommonInsts(true)
                                            .sinkCommonInsts(true)));

    MPM.add(createInstructionCombiningPass());

    // Unroll small loops
    MPM.add(createLoopUnrollPass(CodeGenOpts.OptimizationLevel, false, false));

    // LoopUnroll may generate some redundency to cleanup.
    MPM.add(createInstructionCombiningPass());

    // LoopSink pass sinks instructions hoisted by LICM, which serves as a
    // canonicalization pass that enables other optimizations. As a result,
    // LoopSink pass needs to be a very late IR pass to avoid undoing LICM
    // result too early.
    MPM.add(createLoopSinkPass());
    // Get rid of LCSSA nodes.
    MPM.add(createInstSimplifyLegacyPass());

    // This hoists/decomposes div/rem ops. It should run after other sink/hoist
    // passes to avoid re-sinking, but before SimplifyCFG because it can allow
    // flattening of blocks.
    MPM.add(createDivRemPairsPass());

    // LoopSink (and other loop passes since the last simplifyCFG) might have
    // resulted in single-entry-single-exit or empty blocks. Clean up the CFG.
    MPM.add(createCFGSimplificationPass());
  }
  // PMBuilder.populateModulePassManager(MPM);
}

static void setCommandLineOpts(const CodeGenOptions &CodeGenOpts) {
  SmallVector<const char *, 16> BackendArgs;
  BackendArgs.push_back("clang"); // Fake program name.
  if (!CodeGenOpts.DebugPass.empty()) {
    BackendArgs.push_back("-debug-pass");
    BackendArgs.push_back(CodeGenOpts.DebugPass.c_str());
  }
  if (!CodeGenOpts.LimitFloatPrecision.empty()) {
    BackendArgs.push_back("-limit-float-precision");
    BackendArgs.push_back(CodeGenOpts.LimitFloatPrecision.c_str());
  }
  std::stringstream ss;
  ss << std::numeric_limits<unsigned>::max();
  std::string maxStr = ss.str();
  BackendArgs.push_back("-unroll-threshold-aggressive");
  BackendArgs.push_back(maxStr.c_str());
  BackendArgs.push_back("-unroll-threshold-default");
  BackendArgs.push_back(maxStr.c_str());
  BackendArgs.push_back(nullptr);
  llvm::cl::ParseCommandLineOptions(BackendArgs.size() - 1,
                                    BackendArgs.data());
}

void EmitAssemblyHelper::EmitAssembly(std::unique_ptr<raw_pwrite_stream> OS) {
  TimeRegion Region(CodeGenOpts.TimePasses ? &CodeGenerationTime : nullptr);

  setCommandLineOpts(CodeGenOpts);

  legacy::PassManager PerModulePasses;
  PerModulePasses.add(
      createTargetTransformInfoWrapperPass(getTargetIRAnalysis()));

  legacy::FunctionPassManager PerFunctionPasses(TheModule);
  PerFunctionPasses.add(
      createTargetTransformInfoWrapperPass(getTargetIRAnalysis()));

  CreatePasses(PerModulePasses, PerFunctionPasses);
  
  PerModulePasses.add(new c2ssa::CallGraphCounter(*OS));
  legacy::FunctionPassManager PrintPasses(TheModule);
  PrintPasses.add(
      c2ssa::createPrintModulePass(*OS, ""));

  // Run passes.
  {
    PrettyStackTraceString CrashInfo("Per-function optimization");
    llvm::TimeTraceScope TimeScope("PerFunctionPasses");

    PerFunctionPasses.doInitialization();
    for (Function &F : *TheModule)
      if (!F.isDeclaration())
        PerFunctionPasses.run(F);
    PerFunctionPasses.doFinalization();
  }

  {
    PrettyStackTraceString CrashInfo("Per-module optimization passes");
    llvm::TimeTraceScope TimeScope("PerModulePasses");
    PerModulePasses.run(*TheModule);
  }
  
  {
    PrettyStackTraceString CrashInfo("Printing passes");
    llvm::TimeTraceScope TimeScope("PrintPasses");

    PrintPasses.doInitialization();
    for (Function &F : *TheModule)
      if (!F.isDeclaration())
        PrintPasses.run(F);
    PrintPasses.doFinalization();
  }
}

void c2ssa::EmitBackendOutput(DiagnosticsEngine &Diags,
                              const HeaderSearchOptions &HeaderOpts,
                              const CodeGenOptions &CGOpts,
                              const clang::TargetOptions &TOpts,
                              const LangOptions &LOpts,
                              const llvm::DataLayout &TDesc, Module *M,
                              std::unique_ptr<raw_pwrite_stream> OS) {
  llvm::TimeTraceScope TimeScope("EmitBackendOutput");

  EmitAssemblyHelper AsmHelper(Diags, HeaderOpts, CGOpts, TOpts, LOpts, M);
  AsmHelper.EmitAssembly(std::move(OS));
}
