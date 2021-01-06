#ifndef C2SSA_PROMOTEMEMTOREG_H
#define C2SSA_PROMOTEMEMTOREG_H

namespace llvm {
template <typename T> class ArrayRef;
class AllocaInst;
class DominatorTree;
class AssumptionCache;
}

namespace c2ssa {

/// Return true if this alloca is legal for promotion.
///
/// This is true if there are only loads, stores, and lifetime markers
/// (transitively) using this alloca. This also enforces that there is only
/// ever one layer of bitcasts or GEPs between the alloca and the lifetime
/// markers.
bool isAllocaPromotable(const llvm::AllocaInst *AI);

/// Promote the specified list of alloca instructions into scalar
/// registers, inserting PHI nodes as appropriate.
///
/// This function makes use of DominanceFrontier information.  This function
/// does not modify the CFG of the function at all.  All allocas must be from
/// the same function.
///
void PromoteMemToReg(llvm::ArrayRef<llvm::AllocaInst *> Allocas, llvm::DominatorTree &DT,
                     llvm::AssumptionCache *AC = nullptr);

} // c2ssa

#endif
