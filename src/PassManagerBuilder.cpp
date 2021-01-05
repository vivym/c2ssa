#include "PassManagerBuilder.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/CFLAndersAliasAnalysis.h"
#include "llvm/Analysis/CFLSteensAliasAnalysis.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/ScopedNoAliasAA.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TypeBasedAliasAnalysis.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Transforms/AggressiveInstCombine/AggressiveInstCombine.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/Attributor.h"
#include "llvm/Transforms/IPO/ForceFunctionAttrs.h"
#include "llvm/Transforms/IPO/FunctionAttrs.h"
#include "llvm/Transforms/IPO/InferFunctionAttrs.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Instrumentation.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/InstSimplifyPass.h"
#include "llvm/Transforms/Scalar/LICM.h"
#include "llvm/Transforms/Scalar/LoopUnrollPass.h"
#include "llvm/Transforms/Scalar/SimpleLoopUnswitch.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Vectorize.h"
#include "llvm/Transforms/Vectorize/LoopVectorize.h"
#include "llvm/Transforms/Vectorize/SLPVectorizer.h"
#include "llvm/Transforms/Vectorize/VectorCombine.h"

using namespace llvm;
using namespace c2ssa;

namespace {

bool RunPartialInlining = false;

// Run GVN instead of Early CSE after vectorization passes.
bool UseGVNAfterVectorization = false;

// Run cleanup optimization passes after vectorization.
bool ExtraVectorizerPasses = false;

// Run the loop rerolling pass.
bool RunLoopRerolling = true;

// Run the NewGVN pass.
bool RunNewGVN = false;

// Experimental option to use CFL-AA.
enum class CFLAAType { None, Steensgaard, Andersen, Both };

// Enable the new, experimental CFL alias analysis.
CFLAAType UseCFLAA = CFLAAType::None;

// Enable the new, experimental LoopInterchange Pass.
bool EnableLoopInterchange = true;

// Enable Unroll And Jam Pass.
bool EnableUnrollAndJam = true;

// Enable the LoopFlatten Pass.
bool EnableLoopFlatten = true;

// Enable preparation for ThinLTO.
bool EnablePrepareForThinLTO = false;

// Enable performing ThinLTO.
bool EnablePerformThinLTO = false;

// Enable hot-cold splitting pass.
bool EnableHotColdSplit = false;

// Enable the experimental Loop Versioning LICM pass.
bool UseLoopVersioningLICM = false;

// Disable pre-instrumentation inliner.
bool DisablePreInliner = false;

// Control the amount of inlining in pre-instrumentation inliner.
int PreInlineThreshold = 75;

// Enable the GVN hoisting pass.
bool EnableGVNHoist = false;

// Disable shrink-wrap library calls.
bool DisableLibCallsShrinkWrap = false;

// Enable the simple loop unswitch pass. Also enables independent
// cleanup passes integrated into the loop pass manager pipeline.
bool EnableSimpleLoopUnswitch = false;

// Enable the GVN sinking pass.
bool EnableGVNSink = false;

// This option is used in simplifying testing SampleFDO optimizations for
// profile loading.
// Enable control height reduction optimization (CHR).
bool EnableCHR = true;

// Indicate the sample profile being used is flattened, i.e.,
// no inline hierachy exists in the profile.
bool FlattenedProfileUsed = false;

// Enable order file instrumentation.
bool EnableOrderFileInstrumentation = false;

// Enable lowering of the matrix intrinsics.
bool EnableMatrix = false;

// Enable pass to eliminate conditions based on linear constraints.
bool EnableConstraintElimination = true;

// Enable the attributor inter-procedural deduction pass.
AttributorRunOption AttributorRun = AttributorRunOption::NONE;

bool EnableKnowledgeRetention = false;

}

PassManagerBuilder::PassManagerBuilder() {
    OptLevel = 2;
    SizeLevel = 0;
    LibraryInfo = nullptr;
    Inliner = nullptr;
    DisableUnrollLoops = false;
    SLPVectorize = false;
    LoopVectorize = true;
    LoopsInterleaved = true;
    RerollLoops = RunLoopRerolling;
    NewGVN = RunNewGVN;
    LicmMssaOptCap = SetLicmMssaOptCap;
    LicmMssaNoAccForPromotionCap = SetLicmMssaNoAccForPromotionCap;
    DisableGVNLoadPRE = false;
    ForgetAllSCEVInLoopUnroll = ForgetSCEVInLoopUnroll;
    VerifyInput = false;
    VerifyOutput = false;
    MergeFunctions = false;
    PrepareForLTO = false;
    EnablePGOInstrGen = false;
    EnablePGOCSInstrGen = false;
    EnablePGOCSInstrUse = false;
    PGOInstrGen = "";
    PGOInstrUse = "";
    PGOSampleUse = "";
    PrepareForThinLTO = EnablePrepareForThinLTO;
    PerformThinLTO = EnablePerformThinLTO;
    DivergentTarget = false;
    CallGraphProfile = true;
}

PassManagerBuilder::~PassManagerBuilder() {
  delete LibraryInfo;
  delete Inliner;
}

/// Set of global extensions, automatically added as part of the standard set.
static ManagedStatic<
    SmallVector<std::tuple<PassManagerBuilder::ExtensionPointTy,
                           PassManagerBuilder::ExtensionFn,
                           PassManagerBuilder::GlobalExtensionID>,
                8>>
    GlobalExtensions;
static PassManagerBuilder::GlobalExtensionID GlobalExtensionsCounter;

/// Check if GlobalExtensions is constructed and not empty.
/// Since GlobalExtensions is a managed static, calling 'empty()' will trigger
/// the construction of the object.
static bool GlobalExtensionsNotEmpty() {
  return GlobalExtensions.isConstructed() && !GlobalExtensions->empty();
}

PassManagerBuilder::GlobalExtensionID
PassManagerBuilder::addGlobalExtension(PassManagerBuilder::ExtensionPointTy Ty,
                                       PassManagerBuilder::ExtensionFn Fn) {
  auto ExtensionID = GlobalExtensionsCounter++;
  GlobalExtensions->push_back(std::make_tuple(Ty, std::move(Fn), ExtensionID));
  return ExtensionID;
}

void PassManagerBuilder::removeGlobalExtension(
    PassManagerBuilder::GlobalExtensionID ExtensionID) {
  // RegisterStandardPasses may try to call this function after GlobalExtensions
  // has already been destroyed; doing so should not generate an error.
  if (!GlobalExtensions.isConstructed())
    return;

  auto GlobalExtension =
      llvm::find_if(*GlobalExtensions, [ExtensionID](const auto &elem) {
        return std::get<2>(elem) == ExtensionID;
      });
  assert(GlobalExtension != GlobalExtensions->end() &&
         "The extension ID to be removed should always be valid.");

  GlobalExtensions->erase(GlobalExtension);
}

void PassManagerBuilder::addExtension(ExtensionPointTy Ty, ExtensionFn Fn) {
  Extensions.push_back(std::make_pair(Ty, std::move(Fn)));
}

void PassManagerBuilder::addExtensionsToPM(ExtensionPointTy ETy,
                                           legacy::PassManagerBase &PM) const {
  if (GlobalExtensionsNotEmpty()) {
    for (auto &Ext : *GlobalExtensions) {
      if (std::get<0>(Ext) == ETy)
        std::get<1>(Ext)(*this, PM);
    }
  }
  for (unsigned i = 0, e = Extensions.size(); i != e; ++i)
    if (Extensions[i].first == ETy)
      Extensions[i].second(*this, PM);
}

void PassManagerBuilder::addInitialAliasAnalysisPasses(
    legacy::PassManagerBase &PM) const {
  switch (UseCFLAA) {
  case CFLAAType::Steensgaard:
    PM.add(createCFLSteensAAWrapperPass());
    break;
  case CFLAAType::Andersen:
    PM.add(createCFLAndersAAWrapperPass());
    break;
  case CFLAAType::Both:
    PM.add(createCFLSteensAAWrapperPass());
    PM.add(createCFLAndersAAWrapperPass());
    break;
  default:
    break;
  }

  // Add TypeBasedAliasAnalysis before BasicAliasAnalysis so that
  // BasicAliasAnalysis wins if they disagree. This is intended to help
  // support "obvious" type-punning idioms.
  PM.add(createTypeBasedAAWrapperPass());
  PM.add(createScopedNoAliasAAWrapperPass());
}

void PassManagerBuilder::populateFunctionPassManager(
    legacy::FunctionPassManager &FPM) {
  FPM.add(createEntryExitInstrumenterPass());

  FPM.add(createTypeBasedAAWrapperPass());
  FPM.add(createScopedNoAliasAAWrapperPass());

  FPM.add(createCFGSimplificationPass());
  FPM.add(createSROAPass());
  FPM.add(createEarlyCSEPass());
  FPM.add(createLowerExpectIntrinsicPass());
}

// Do PGO instrumentation generation or use pass as the option specified.
void PassManagerBuilder::addPGOInstrPasses(legacy::PassManagerBase &MPM,
                                           bool IsCS = false) {
  if (IsCS) {
    if (!EnablePGOCSInstrGen && !EnablePGOCSInstrUse)
      return;
  } else if (!EnablePGOInstrGen && PGOInstrUse.empty() && PGOSampleUse.empty())
    return;

  // Perform the preinline and cleanup passes for O1 and above.
  // We will not do this inline for context sensitive PGO (when IsCS is true).
  if (OptLevel > 0 && !DisablePreInliner && PGOSampleUse.empty() && !IsCS) {
    // Create preinline pass. We construct an InlineParams object and specify
    // the threshold here to avoid the command line options of the regular
    // inliner to influence pre-inlining. The only fields of InlineParams we
    // care about are DefaultThreshold and HintThreshold.
    InlineParams IP;
    IP.DefaultThreshold = PreInlineThreshold;
    // FIXME: The hint threshold has the same value used by the regular inliner
    // when not optimzing for size. This should probably be lowered after
    // performance testing.
    // Use PreInlineThreshold for both -Os and -Oz. Not running preinliner makes
    // the instrumented binary unusably large. Even if PreInlineThreshold is not
    // correct thresold for -Oz, it is better than not running preinliner.
    IP.HintThreshold = SizeLevel > 0 ? PreInlineThreshold : 325;

    MPM.add(createFunctionInliningPass(IP));
    MPM.add(createSROAPass());
    MPM.add(createEarlyCSEPass());             // Catch trivial redundancies
    MPM.add(createCFGSimplificationPass());    // Merge & remove BBs
    MPM.add(createInstructionCombiningPass()); // Combine silly seq's
    addExtensionsToPM(EP_Peephole, MPM);
  }
  if ((EnablePGOInstrGen && !IsCS) || (EnablePGOCSInstrGen && IsCS)) {
    MPM.add(createPGOInstrumentationGenLegacyPass(IsCS));
    // Add the profile lowering pass.
    InstrProfOptions Options;
    if (!PGOInstrGen.empty())
      Options.InstrProfileOutput = PGOInstrGen;
    Options.DoCounterPromotion = true;
    Options.UseBFIInPromotion = IsCS;
    MPM.add(createLoopRotatePass());
    MPM.add(createInstrProfilingLegacyPass(Options, IsCS));
  }
  if (!PGOInstrUse.empty())
    MPM.add(createPGOInstrumentationUseLegacyPass(PGOInstrUse, IsCS));
  // Indirect call promotion that promotes intra-module targets only.
  // For ThinLTO this is done earlier due to interactions with globalopt
  // for imported functions. We don't run this at -O0.
  if (OptLevel > 0 && !IsCS)
    MPM.add(
        createPGOIndirectCallPromotionLegacyPass(false, !PGOSampleUse.empty()));
}
void PassManagerBuilder::addFunctionSimplificationPasses(
    legacy::PassManagerBase &MPM) {
  // Start of function pass.
  // Break up aggregate allocas, using SSAUpdater.
  assert(OptLevel >= 1 && "Calling function optimizer with no optimization level!");
  MPM.add(createSROAPass());
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
  // Rotate Loop - disable header duplication at -Oz
  MPM.add(createLoopRotatePass(SizeLevel == 2 ? 0 : -1));
  // TODO: Investigate promotion cap for O1.
  MPM.add(createLICMPass(LicmMssaOptCap, LicmMssaNoAccForPromotionCap));
  MPM.add(createLoopUnswitchPass(SizeLevel || OptLevel < 3, DivergentTarget));
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
  MPM.add(createSimpleLoopUnrollPass(OptLevel, DisableUnrollLoops,
                                     ForgetAllSCEVInLoopUnroll));
  // This ends the loop pass pipelines.

  // Break up allocas that may now be splittable after loop unrolling.
  MPM.add(createSROAPass());

  MPM.add(createMergedLoadStoreMotionPass()); // Merge ld/st in diamonds
  MPM.add(createGVNPass(DisableGVNLoadPRE)); // Remove redundancies
  MPM.add(createMemCpyOptPass());             // Remove memcpy / form memset
  MPM.add(createSCCPPass());                  // Constant prop with SCCP

  if (EnableConstraintElimination) // RUN
    MPM.add(createConstraintEliminationPass());

  // Delete dead bit computations (instcombine runs after to fold away the dead
  // computations, and then ADCE will run later to exploit any new DCE
  // opportunities that creates).
  MPM.add(createBitTrackingDCEPass());        // Delete dead bit computations

  // Run instcombine after redundancy elimination to exploit opportunities
  // opened up by them.
  MPM.add(createInstructionCombiningPass());
  addExtensionsToPM(EP_Peephole, MPM); // DEL
  if (OptLevel > 1) {
    MPM.add(createJumpThreadingPass());         // Thread jumps
    MPM.add(createCorrelatedValuePropagationPass());
  }
  MPM.add(createAggressiveDCEPass()); // Delete dead instructions

  // TODO: Investigate if this is too expensive at O1.
  if (OptLevel > 1) {
    MPM.add(createDeadStoreEliminationPass());  // Delete dead stores
    MPM.add(createLICMPass(LicmMssaOptCap, LicmMssaNoAccForPromotionCap));
  }

  addExtensionsToPM(EP_ScalarOptimizerLate, MPM); // DEL

  if (RerollLoops)
    MPM.add(createLoopRerollPass());

  MPM.add(createCFGSimplificationPass()); // Merge & remove BBs
  // Clean up after everything.
  MPM.add(createInstructionCombiningPass());
  addExtensionsToPM(EP_Peephole, MPM); // DEL

  if (EnableCHR && OptLevel >= 3 &&
      (!PGOInstrUse.empty() || !PGOSampleUse.empty() || EnablePGOCSInstrGen)) // DEL
    MPM.add(createControlHeightReductionLegacyPass());
}

void PassManagerBuilder::populateModulePassManager(
    legacy::PassManagerBase &MPM) {
  addInitialAliasAnalysisPasses(MPM);

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
  MPM.add(createLoopUnrollPass(OptLevel, DisableUnrollLoops,
                               ForgetAllSCEVInLoopUnroll));

  // LoopUnroll may generate some redundency to cleanup.
  MPM.add(createInstructionCombiningPass());

  // Runtime unrolling will introduce runtime check in loop prologue. If the
  // unrolled loop is a inner loop, then the prologue will be inside the
  // outer loop. LICM pass can help to promote the runtime check out if the
  // checked value is loop invariant.
  MPM.add(createLICMPass(LicmMssaOptCap, LicmMssaNoAccForPromotionCap));

  // After vectorization and unrolling, assume intrinsics may tell us more
  // about pointer alignments.
  MPM.add(createAlignmentFromAssumptionsPass());

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

void PassManagerBuilder::addLTOOptimizationPasses(legacy::PassManagerBase &PM) {
  // Load sample profile before running the LTO optimization pipeline.
  if (!PGOSampleUse.empty()) {
    PM.add(createPruneEHPass());
    PM.add(createSampleProfileLoaderPass(PGOSampleUse));
  }

  // Remove unused virtual tables to improve the quality of code generated by
  // whole-program devirtualization and bitset lowering.
  PM.add(createGlobalDCEPass());

  // Provide AliasAnalysis services for optimizations.
  addInitialAliasAnalysisPasses(PM);

  // Allow forcing function attributes as a debugging and tuning aid.
  PM.add(createForceFunctionAttrsLegacyPass());

  // Infer attributes about declarations if possible.
  PM.add(createInferFunctionAttrsLegacyPass());

  if (OptLevel > 1) {
    // Split call-site with more constrained arguments.
    PM.add(createCallSiteSplittingPass());

    // Indirect call promotion. This should promote all the targets that are
    // left by the earlier promotion pass that promotes intra-module targets.
    // This two-step promotion is to save the compile time. For LTO, it should
    // produce the same result as if we only do promotion here.
    PM.add(
        createPGOIndirectCallPromotionLegacyPass(true, !PGOSampleUse.empty()));

    // Propagate constants at call sites into the functions they call.  This
    // opens opportunities for globalopt (and inlining) by substituting function
    // pointers passed as arguments to direct uses of functions.
    PM.add(createIPSCCPPass());

    // Attach metadata to indirect call sites indicating the set of functions
    // they may target at run-time. This should follow IPSCCP.
    PM.add(createCalledValuePropagationPass());

    // Infer attributes on declarations, call sites, arguments, etc.
    if (AttributorRun & AttributorRunOption::MODULE)
      PM.add(createAttributorLegacyPass());
  }

  // Infer attributes about definitions. The readnone attribute in particular is
  // required for virtual constant propagation.
  PM.add(createPostOrderFunctionAttrsLegacyPass());
  PM.add(createReversePostOrderFunctionAttrsPass());

  // Split globals using inrange annotations on GEP indices. This can help
  // improve the quality of generated code when virtual constant propagation or
  // control flow integrity are enabled.
  PM.add(createGlobalSplitPass());

  // Apply whole-program devirtualization and virtual constant propagation.
  PM.add(createWholeProgramDevirtPass(ExportSummary, nullptr));

  // That's all we need at opt level 1.
  if (OptLevel == 1)
    return;

  // Now that we internalized some globals, see if we can hack on them!
  PM.add(createGlobalOptimizerPass());
  // Promote any localized global vars.
  PM.add(createPromoteMemoryToRegisterPass());

  // Linking modules together can lead to duplicated global constants, only
  // keep one copy of each constant.
  PM.add(createConstantMergePass());

  // Remove unused arguments from functions.
  PM.add(createDeadArgEliminationPass());

  // Reduce the code after globalopt and ipsccp.  Both can open up significant
  // simplification opportunities, and both can propagate functions through
  // function pointers.  When this happens, we often have to resolve varargs
  // calls, etc, so let instcombine do this.
  if (OptLevel > 2)
    PM.add(createAggressiveInstCombinerPass());
  PM.add(createInstructionCombiningPass());
  addExtensionsToPM(EP_Peephole, PM);

  // Inline small functions
  bool RunInliner = Inliner;
  if (RunInliner) {
    PM.add(Inliner);
    Inliner = nullptr;
  }

  PM.add(createPruneEHPass());   // Remove dead EH info.

  // CSFDO instrumentation and use pass.
  addPGOInstrPasses(PM, /* IsCS */ true);

  // Infer attributes on declarations, call sites, arguments, etc. for an SCC.
  if (AttributorRun & AttributorRunOption::CGSCC)
    PM.add(createAttributorCGSCCLegacyPass());

  // Try to perform OpenMP specific optimizations. This is a (quick!) no-op if
  // there are no OpenMP runtime calls present in the module.
  if (OptLevel > 1)
    PM.add(createOpenMPOptLegacyPass());

  // Optimize globals again if we ran the inliner.
  if (RunInliner)
    PM.add(createGlobalOptimizerPass());
  PM.add(createGlobalDCEPass()); // Remove dead functions.

  // If we didn't decide to inline a function, check to see if we can
  // transform it to pass arguments by value instead of by reference.
  PM.add(createArgumentPromotionPass());

  // The IPO passes may leave cruft around.  Clean up after them.
  PM.add(createInstructionCombiningPass());
  addExtensionsToPM(EP_Peephole, PM);
  PM.add(createJumpThreadingPass(/*FreezeSelectCond*/ true));

  // Break up allocas
  PM.add(createSROAPass());

  // LTO provides additional opportunities for tailcall elimination due to
  // link-time inlining, and visibility of nocapture attribute.
  if (OptLevel > 1)
    PM.add(createTailCallEliminationPass());

  // Infer attributes on declarations, call sites, arguments, etc.
  PM.add(createPostOrderFunctionAttrsLegacyPass()); // Add nocapture.
  // Run a few AA driven optimizations here and now, to cleanup the code.
  PM.add(createGlobalsAAWrapperPass()); // IP alias analysis.

  PM.add(createLICMPass(LicmMssaOptCap, LicmMssaNoAccForPromotionCap));
  PM.add(NewGVN ? createNewGVNPass()
                : createGVNPass(DisableGVNLoadPRE)); // Remove redundancies.
  PM.add(createMemCpyOptPass());            // Remove dead memcpys.

  // Nuke dead stores.
  PM.add(createDeadStoreEliminationPass());
  PM.add(createMergedLoadStoreMotionPass()); // Merge ld/st in diamonds.

  // More loops are countable; try to optimize them.
  if (EnableLoopFlatten)
    PM.add(createLoopFlattenPass());
  PM.add(createIndVarSimplifyPass());
  PM.add(createLoopDeletionPass());
  PM.add(createLoopInterchangePass());

  PM.add(createConstraintEliminationPass());

  // Unroll small loops
  PM.add(createSimpleLoopUnrollPass(OptLevel, DisableUnrollLoops,
                                    ForgetAllSCEVInLoopUnroll));
  PM.add(createLoopDistributePass());
  PM.add(createLoopVectorizePass(true, !LoopVectorize));
  // The vectorizer may have significantly shortened a loop body; unroll again.
  PM.add(createLoopUnrollPass(OptLevel, DisableUnrollLoops,
                              ForgetAllSCEVInLoopUnroll));

  // Now that we've optimized loops (in particular loop induction variables),
  // we may have exposed more scalar opportunities. Run parts of the scalar
  // optimizer again at this point.
  PM.add(createInstructionCombiningPass()); // Initial cleanup
  PM.add(createCFGSimplificationPass()); // if-convert
  PM.add(createSCCPPass()); // Propagate exposed constants
  PM.add(createInstructionCombiningPass()); // Clean up again
  PM.add(createBitTrackingDCEPass());

  // Cleanup and simplify the code after the scalar optimizations.
  PM.add(createInstructionCombiningPass());
}

void PassManagerBuilder::addLateLTOOptimizationPasses(
    legacy::PassManagerBase &PM) {
  // See comment in the new PM for justification of scheduling splitting at
  // this stage (\ref buildLTODefaultPipeline).
  if (EnableHotColdSplit)
    PM.add(createHotColdSplittingPass());

  // Delete basic blocks, which optimization passes may have killed.
  PM.add(createCFGSimplificationPass());

  // Drop bodies of available externally objects to improve GlobalDCE.
  PM.add(createEliminateAvailableExternallyPass());

  // Now that we have optimized the program, discard unreachable functions.
  PM.add(createGlobalDCEPass());

  // FIXME: this is profitable (for compiler time) to do at -O0 too, but
  // currently it damages debug info.
  if (MergeFunctions)
    PM.add(createMergeFunctionsPass());
}

void PassManagerBuilder::populateThinLTOPassManager(
    legacy::PassManagerBase &PM) {
  PerformThinLTO = true;
  if (LibraryInfo)
    PM.add(new TargetLibraryInfoWrapperPass(*LibraryInfo));

  if (VerifyInput)
    PM.add(createVerifierPass());

  if (ImportSummary) {
    // This pass imports type identifier resolutions for whole-program
    // devirtualization and CFI. It must run early because other passes may
    // disturb the specific instruction patterns that these passes look for,
    // creating dependencies on resolutions that may not appear in the summary.
    //
    // For example, GVN may transform the pattern assume(type.test) appearing in
    // two basic blocks into assume(phi(type.test, type.test)), which would
    // transform a dependency on a WPD resolution into a dependency on a type
    // identifier resolution for CFI.
    //
    // Also, WPD has access to more precise information than ICP and can
    // devirtualize more effectively, so it should operate on the IR first.
    PM.add(createWholeProgramDevirtPass(nullptr, ImportSummary));
    PM.add(createLowerTypeTestsPass(nullptr, ImportSummary));
  }

  populateModulePassManager(PM);

  if (VerifyOutput)
    PM.add(createVerifierPass());
  PerformThinLTO = false;
}

void PassManagerBuilder::populateLTOPassManager(legacy::PassManagerBase &PM) {
  if (LibraryInfo)
    PM.add(new TargetLibraryInfoWrapperPass(*LibraryInfo));

  if (VerifyInput)
    PM.add(createVerifierPass());

  addExtensionsToPM(EP_FullLinkTimeOptimizationEarly, PM);

  if (OptLevel != 0)
    addLTOOptimizationPasses(PM);
  else {
    // The whole-program-devirt pass needs to run at -O0 because only it knows
    // about the llvm.type.checked.load intrinsic: it needs to both lower the
    // intrinsic itself and handle it in the summary.
    PM.add(createWholeProgramDevirtPass(ExportSummary, nullptr));
  }

  // Create a function that performs CFI checks for cross-DSO calls with targets
  // in the current module.
  PM.add(createCrossDSOCFIPass());

  // Lower type metadata and the type.test intrinsic. This pass supports Clang's
  // control flow integrity mechanisms (-fsanitize=cfi*) and needs to run at
  // link time if CFI is enabled. The pass does nothing if CFI is disabled.
  PM.add(createLowerTypeTestsPass(ExportSummary, nullptr));
  // Run a second time to clean up any type tests left behind by WPD for use
  // in ICP (which is performed earlier than this in the regular LTO pipeline).
  PM.add(createLowerTypeTestsPass(nullptr, nullptr, true));

  if (OptLevel != 0)
    addLateLTOOptimizationPasses(PM);

  addExtensionsToPM(EP_FullLinkTimeOptimizationLast, PM);

  PM.add(createAnnotationRemarksLegacyPass());

  if (VerifyOutput)
    PM.add(createVerifierPass());
}
