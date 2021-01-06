#ifndef C2SSA_SROA_H
#define C2SSA_SROA_H

#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/PassManager.h"
#include <vector>

namespace llvm {
class AllocaInst;
class AssumptionCache;
class DominatorTree;
class Function;
class Instruction;
class LLVMContext;
class PHINode;
class SelectInst;
class Use;
}

namespace c2ssa {

/// A private "module" namespace for types and utilities used by SROA. These
/// are implementation details and should not be used by clients.
namespace sroa LLVM_LIBRARY_VISIBILITY {

class AllocaSliceRewriter;
class AllocaSlices;
class Partition;
class SROAPass;

} // end namespace sroa

/// An optimization pass providing Scalar Replacement of Aggregates.
///
/// This pass takes allocations which can be completely analyzed (that is, they
/// don't escape) and tries to turn them into scalar SSA values. There are
/// a few steps to this process.
///
/// 1) It takes allocations of aggregates and analyzes the ways in which they
///    are used to try to split them into smaller allocations, ideally of
///    a single scalar data type. It will split up memcpy and memset accesses
///    as necessary and try to isolate individual scalar accesses.
/// 2) It will transform accesses into forms which are suitable for SSA value
///    promotion. This can be replacing a memset with a scalar store of an
///    integer value, or it can involve speculating operations on a PHI or
///    select to be a PHI or select of the results.
/// 3) Finally, this will try to detect a pattern of accesses which map cleanly
///    onto insert and extract operations on a vector value, and convert them to
///    this form. By doing so, it will enable promotion of vector aggregates to
///    SSA vector values.
class SROA : public llvm::PassInfoMixin<SROA> {
  llvm::LLVMContext *C = nullptr;
  llvm::DominatorTree *DT = nullptr;
  llvm::AssumptionCache *AC = nullptr;

  /// Worklist of alloca instructions to simplify.
  ///
  /// Each alloca in the function is added to this. Each new alloca formed gets
  /// added to it as well to recursively simplify unless that alloca can be
  /// directly promoted. Finally, each time we rewrite a use of an alloca other
  /// the one being actively rewritten, we add it back onto the list if not
  /// already present to ensure it is re-visited.
  llvm::SetVector<llvm::AllocaInst *, llvm::SmallVector<llvm::AllocaInst *, 16>> Worklist;

  /// A collection of instructions to delete.
  /// We try to batch deletions to simplify code and make things a bit more
  /// efficient.
  llvm::SetVector<llvm::Instruction *, llvm::SmallVector<llvm::Instruction *, 8>> DeadInsts;

  /// Post-promotion worklist.
  ///
  /// Sometimes we discover an alloca which has a high probability of becoming
  /// viable for SROA after a round of promotion takes place. In those cases,
  /// the alloca is enqueued here for re-processing.
  ///
  /// Note that we have to be very careful to clear allocas out of this list in
  /// the event they are deleted.
  llvm::SetVector<llvm::AllocaInst *, llvm::SmallVector<llvm::AllocaInst *, 16>> PostPromotionWorklist;

  /// A collection of alloca instructions we can directly promote.
  std::vector<llvm::AllocaInst *> PromotableAllocas;

  /// A worklist of PHIs to speculate prior to promoting allocas.
  ///
  /// All of these PHIs have been checked for the safety of speculation and by
  /// being speculated will allow promoting allocas currently in the promotable
  /// queue.
  llvm::SetVector<llvm::PHINode *, llvm::SmallVector<llvm::PHINode *, 2>> SpeculatablePHIs;

  /// A worklist of select instructions to speculate prior to promoting
  /// allocas.
  ///
  /// All of these select instructions have been checked for the safety of
  /// speculation and by being speculated will allow promoting allocas
  /// currently in the promotable queue.
  llvm::SetVector<llvm::SelectInst *, llvm::SmallVector<llvm::SelectInst *, 2>> SpeculatableSelects;

public:
  SROA() = default;

  /// Run the pass over the function.
  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &AM);

private:
  friend class sroa::AllocaSliceRewriter;
  friend class sroa::SROAPass;

  /// Helper used by both the public run method and by the legacy pass.
  llvm::PreservedAnalyses runImpl(llvm::Function &F, llvm::DominatorTree &RunDT,
                                  llvm::AssumptionCache &RunAC);

  bool presplitLoadsAndStores(llvm::AllocaInst &AI, sroa::AllocaSlices &AS);
  llvm::AllocaInst *rewritePartition(llvm::AllocaInst &AI, sroa::AllocaSlices &AS,
                               sroa::Partition &P);
  bool splitAlloca(llvm::AllocaInst &AI, sroa::AllocaSlices &AS);
  bool runOnAlloca(llvm::AllocaInst &AI);
  void clobberUse(llvm::Use &U);
  bool deleteDeadInstructions(llvm::SmallPtrSetImpl<llvm::AllocaInst *> &DeletedAllocas);
  bool promoteAllocas(llvm::Function &F);
};

llvm::FunctionPass *createSROAPass();

void initializeSROAPassPass(llvm::PassRegistry &Registry);

} // end namespace c2ssa

#endif // C2SSA_SROA_H
