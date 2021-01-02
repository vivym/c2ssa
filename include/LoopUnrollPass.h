#ifndef C2SSA_LOOPUNROLLPASS_H
#define C2SSA_LOOPUNROLLPASS_H

#include "llvm/ADT/Optional.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Support/CommandLine.h"

namespace llvm {
class Function;
class Loop;
class LPMUpdater;
}

namespace c2ssa {

/// A set of parameters used to control various transforms performed by the
/// LoopUnroll pass. Each of the boolean parameters can be set to:
///      true - enabling the transformation.
///      false - disabling the transformation.
///      None - relying on a global default.
///
/// There is also OptLevel parameter, which is used for additional loop unroll
/// tuning.
///
/// Intended use is to create a default object, modify parameters with
/// additional setters and then pass it to LoopUnrollPass.
///

struct LoopUnrollOptions {
  llvm::Optional<bool> AllowPartial;
  llvm::Optional<bool> AllowPeeling;
  llvm::Optional<bool> AllowRuntime;
  llvm::Optional<bool> AllowUpperBound;
  llvm::Optional<bool> AllowProfileBasedPeeling;
  llvm::Optional<unsigned> FullUnrollMaxCount;
  int OptLevel;

  /// If false, use a cost model to determine whether unrolling of a loop is
  /// profitable. If true, only loops that explicitly request unrolling via
  /// metadata are considered. All other loops are skipped.
  bool OnlyWhenForced;

  /// If true, forget all loops when unrolling. If false, forget top-most loop
  /// of the currently processed loops, which removes one entry at a time from
  /// the internal SCEV records. For large loops, the former is faster.
  const bool ForgetSCEV;

  LoopUnrollOptions(int OptLevel = 2, bool OnlyWhenForced = false,
                    bool ForgetSCEV = false)
      : OptLevel(OptLevel), OnlyWhenForced(OnlyWhenForced),
        ForgetSCEV(ForgetSCEV) {}

  /// Enables or disables partial unrolling. When disabled only full unrolling
  /// is allowed.
  LoopUnrollOptions &setPartial(bool Partial) {
    AllowPartial = Partial;
    return *this;
  }

  /// Enables or disables unrolling of loops with runtime trip count.
  LoopUnrollOptions &setRuntime(bool Runtime) {
    AllowRuntime = Runtime;
    return *this;
  }

  /// Enables or disables loop peeling.
  LoopUnrollOptions &setPeeling(bool Peeling) {
    AllowPeeling = Peeling;
    return *this;
  }

  /// Enables or disables the use of trip count upper bound
  /// in loop unrolling.
  LoopUnrollOptions &setUpperBound(bool UpperBound) {
    AllowUpperBound = UpperBound;
    return *this;
  }

  // Sets "optimization level" tuning parameter for loop unrolling.
  LoopUnrollOptions &setOptLevel(int O) {
    OptLevel = O;
    return *this;
  }

  // Enables or disables loop peeling basing on profile.
  LoopUnrollOptions &setProfileBasedPeeling(int O) {
    AllowProfileBasedPeeling = O;
    return *this;
  }

  // Sets the max full unroll count.
  LoopUnrollOptions &setFullUnrollMaxCount(unsigned O) {
    FullUnrollMaxCount = O;
    return *this;
  }
};

/// Loop unroll pass that will support both full and partial unrolling.
/// It is a function pass to have access to function and module analyses.
/// It will also put loops into canonical form (simplified and LCSSA).
class LoopUnrollPass : public llvm::PassInfoMixin<LoopUnrollPass> {
  LoopUnrollOptions UnrollOpts;

public:
  /// This uses the target information (or flags) to control the thresholds for
  /// different unrolling stategies but supports all of them.
  explicit LoopUnrollPass(LoopUnrollOptions UnrollOpts = {})
      : UnrollOpts(UnrollOpts) {}

  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &AM);
};

} // end namespace c2ssa

#endif // C2SSA_LOOPUNROLLPASS_H
