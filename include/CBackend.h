#ifndef C2SSA_CBACKEND_H
#define C2SSA_CBACKEND_H

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
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/CodeGen/IntrinsicLowering.h"
#include "IDMap.h"
#include <set>

namespace c2ssa {

class CWriter : public llvm::InstVisitor<CWriter> {
private:
  using raw_ostream = llvm::raw_ostream;
  
  raw_ostream &Out;
  llvm::IntrinsicLowering *IL = nullptr;
  llvm::LoopInfo *LI = nullptr;
  const llvm::DataLayout *TD = nullptr;
  const llvm::Instruction *CurInstr = nullptr;

  IDMap<const llvm::ConstantFP *> FPConstantMap;
  std::set<const llvm::Argument *> ByValParams;

  IDMap<const llvm::Value *> AnonValueNumbers;
  
  IDMap<llvm::StructType *> UnnamedStructIDs;
  
  std::set<llvm::Type *> TypedefDeclTypes;
  std::set<llvm::Type *> SelectDeclTypes;
  std::set<std::pair<llvm::CmpInst::Predicate, llvm::VectorType *>> CmpDeclTypes;
  std::set<std::pair<llvm::CastInst::CastOps, std::pair<llvm::Type *, llvm::Type *>>>
      CastOpDeclTypes;
  std::set<std::pair<unsigned, llvm::Type *>> InlineOpDeclTypes;
  std::set<llvm::Type *> CtorDeclTypes;

  IDMap<std::pair<llvm::FunctionType *, std::pair<llvm::AttributeList, llvm::CallingConv::ID>>>
      UnnamedFunctionIDs;
  /// UnnamedStructIDs - This contains a unique ID for each struct that is
  /// either anonymous or has no name.
  
  // This is used to keep track of intrinsics that get generated to a lowered
  // function. We must generate the prototypes before the function body which
  // will only be expanded on first use
  std::vector<Function *> prototypesToGen;
  
  unsigned LastAnnotatedSourceLine = 0;
  
  struct {
    bool BuiltinAlloca : 1;
    bool Unreachable : 1;
    bool NoReturn : 1;
    bool ExternalWeak : 1;
    bool AttributeWeak : 1;
    bool Hidden : 1;
    bool AttributeList : 1;
    bool UnalignedLoad : 1;
    bool MsAlign : 1;
    bool NanInf : 1;
    bool Int128 : 1;
    bool ThreadFence : 1;
    bool StackSaveRestore : 1;
    bool ConstantDoubleTy : 1;
    bool ConstantFloatTy : 1;
    bool ConstantFP80Ty : 1;
    bool ConstantFP128Ty : 1;
    bool BitCastUnion : 1;
    bool ForceInline : 1;
  } UsedHeaders;
  
#define USED_HEADERS_FLAG(Name)                                                \
  void headerUse##Name() { UsedHeaders.Name = true; }                          \
  bool headerInc##Name() const { return UsedHeaders.Name; }

  USED_HEADERS_FLAG(BuiltinAlloca)
  USED_HEADERS_FLAG(Unreachable)
  USED_HEADERS_FLAG(NoReturn)
  USED_HEADERS_FLAG(ExternalWeak)
  USED_HEADERS_FLAG(AttributeWeak)
  USED_HEADERS_FLAG(Hidden)
  USED_HEADERS_FLAG(AttributeList)
  USED_HEADERS_FLAG(UnalignedLoad)
  USED_HEADERS_FLAG(MsAlign)
  USED_HEADERS_FLAG(NanInf)
  USED_HEADERS_FLAG(Int128)
  USED_HEADERS_FLAG(ThreadFence)
  USED_HEADERS_FLAG(StackSaveRestore)
  USED_HEADERS_FLAG(ConstantDoubleTy)
  USED_HEADERS_FLAG(ConstantFloatTy)
  USED_HEADERS_FLAG(ConstantFP80Ty)
  USED_HEADERS_FLAG(ConstantFP128Ty)
  USED_HEADERS_FLAG(BitCastUnion)
  USED_HEADERS_FLAG(ForceInline)
  
  llvm::SmallSet<llvm::CmpInst::Predicate, 26> FCmpOps;
  void headerUseFCmpOp(llvm::CmpInst::Predicate P);
  
  void generateCompilerSpecificCode(raw_ostream &Out, const DataLayout *) const;
  
public:
  explicit CWriter(llvm::raw_ostream &Out): Out(Out) {
    memset(&UsedHeaders, 0, sizeof(UsedHeaders));
  }
  
  void printFunction(llvm::Function &, llvm::LoopInfo *, const llvm::DataLayout *);
  
private:
  void generateHeader(Module &M);
  void declareOneGlobalVariable(GlobalVariable *I);

  void forwardDeclareStructs(raw_ostream &Out, Type *Ty,
                             std::set<Type *> &TypesPrinted);
  
  raw_ostream &printFunctionAttributes(llvm::raw_ostream &Out, llvm::AttributeList Attrs);
  
  raw_ostream &
  printFunctionProto(llvm::raw_ostream &Out, llvm::FunctionType *Ty,
                     std::pair<llvm::AttributeList, llvm::CallingConv::ID> Attrs,
                     const std::string &Name,
                     iterator_range<llvm::Function::arg_iterator> *ArgList);
  raw_ostream &printFunctionProto(llvm::raw_ostream &Out, llvm::Function *F) {
    return printFunctionProto(
        Out, F->getFunctionType(),
        std::make_pair(F->getAttributes(), F->getCallingConv()),
        GetValueName(F), nullptr);
  }
  
  raw_ostream &
  printFunctionDeclaration(llvm::raw_ostream &Out, llvm::FunctionType *Ty,
                           std::pair<llvm::AttributeList, llvm::CallingConv::ID> PAL =
                               std::make_pair(llvm::AttributeList(), llvm::CallingConv::C));
  raw_ostream &printStructDeclaration(llvm::raw_ostream &Out, llvm::StructType *Ty);
  raw_ostream &printArrayDeclaration(llvm::raw_ostream &Out, llvm::ArrayType *Ty);
  
  raw_ostream &printTypeName(raw_ostream &Out, llvm::Type *Ty, bool isSigned = false,
                             std::pair<llvm::AttributeList, llvm::CallingConv::ID> PAL =
                                 std::make_pair(llvm::AttributeList(),
                                                llvm::CallingConv::C));
  raw_ostream &printTypeNameUnaligned(raw_ostream &Out, llvm::Type *Ty,
                                      bool isSigned = false);
  raw_ostream &printSimpleType(raw_ostream &Out, llvm::Type *Ty, bool isSigned);
  raw_ostream &printTypeString(raw_ostream &Out, llvm::Type *Ty, bool isSigned);

  std::string getStructName(llvm::StructType *ST);
  std::string getFunctionName(llvm::FunctionType *FT,
                              std::pair<llvm::AttributeList, llvm::CallingConv::ID> PAL =
                                  std::make_pair(llvm::AttributeList(),
                                                 llvm::CallingConv::C));
  std::string getArrayName(llvm::ArrayType *AT);
  
  enum OperandContext {
    ContextNormal,
    ContextCasted,
    // Casted context means the type-cast will be implicit,
    // such as the RHS of a `var = RHS;` expression
    // or inside a struct initializer expression
    ContextStatic
    // Static context means that it is being used in as a static initializer
    // (also implies ContextCasted)
  };
  
  void writeOperandDeref(llvm::Value *Operand);
  void writeOperand(llvm::Value *Operand,
                    enum OperandContext Context = ContextNormal);
  void writeInstComputationInline(llvm::Instruction &I);
  void writeOperandInternal(llvm::Value *Operand,
                            enum OperandContext Context = ContextNormal);
  void writeOperandWithCast(llvm::Value *Operand, unsigned Opcode);
  void opcodeNeedsCast(unsigned Opcode, bool &shouldCast, bool &castIsSigned);

  void writeOperandWithCast(llvm::Value *Operand, llvm::ICmpInst &I);
  bool writeInstructionCast(llvm::Instruction &I);
  void writeMemoryAccess(llvm::Value *Operand, llvm::Type *OperandType, bool IsVolatile,
                         unsigned Alignment);

  bool lowerIntrinsics(Function &F);
  /// Prints the definition of the intrinsic function F. Supports the
  /// intrinsics which need to be explicitly defined in the CBackend.
  void printIntrinsicDefinition(Function &F, raw_ostream &Out);
  void printIntrinsicDefinition(FunctionType *funT, unsigned Opcode,
                                std::string OpName, raw_ostream &Out);
  
  void printModuleTypes(raw_ostream &Out);
  void printContainedTypes(raw_ostream &Out, Type *Ty, std::set<Type *> &);
  
  void printFloatingPointConstants(llvm::Function &F);
  void printFloatingPointConstants(const llvm::Constant *C);

  void printBasicBlock(llvm::BasicBlock *BB);
  void printLoop(llvm::Loop *L);
  
  void printCast(unsigned opcode, Type *SrcTy, Type *DstTy);
  void printConstant(llvm::Constant *CPV, enum OperandContext Context);
  void printConstantWithCast(llvm::Constant *CPV, unsigned Opcode);
  bool printConstExprCast(llvm::ConstantExpr *CE);
  void printConstantArray(llvm::ConstantArray *CPA, enum OperandContext Context);
  void printConstantVector(llvm::ConstantVector *CV, enum OperandContext Context);
  void printConstantDataSequential(llvm::ConstantDataSequential *CDS,
                                   enum OperandContext Context);
  bool printConstantString(llvm::Constant *C, enum OperandContext Context);

  bool isEmptyType(llvm::Type *Ty) const;
  bool isAddressExposed(llvm::Value *V) const;
  bool isInlinableInst(llvm::Instruction &I) const;
  AllocaInst *isDirectAlloca(llvm::Value *V) const;
  bool isInlineAsm(llvm::Instruction &I) const;
  
  // Instruction visitation functions
  friend class llvm::InstVisitor<CWriter>;

  void visitReturnInst(llvm::ReturnInst &I);
  void visitBranchInst(llvm::BranchInst &I);
  void visitSwitchInst(llvm::SwitchInst &I);
  void visitIndirectBrInst(llvm::IndirectBrInst &I);
  void visitInvokeInst(llvm::InvokeInst &I) {
    llvm_unreachable("Lowerinvoke pass didn't work!");
  }
  void visitResumeInst(llvm::ResumeInst &I) {
    llvm_unreachable("DwarfEHPrepare pass didn't work!");
  }
  void visitUnreachableInst(llvm::UnreachableInst &I);

  void visitPHINode(llvm::PHINode &I);
  void visitBinaryOperator(llvm::BinaryOperator &I);
  void visitICmpInst(llvm::ICmpInst &I);
  void visitFCmpInst(llvm::FCmpInst &I);

  void visitCastInst(llvm::CastInst &I);
  void visitSelectInst(llvm::SelectInst &I);
  void visitCallInst(llvm::CallInst &I);
  void visitInlineAsm(llvm::CallInst &I);
  bool visitBuiltinCall(llvm::CallInst &I, llvm::Intrinsic::ID ID);

  void visitAllocaInst(llvm::AllocaInst &I);
  void visitLoadInst(llvm::LoadInst &I);
  void visitStoreInst(llvm::StoreInst &I);
  void visitFenceInst(llvm::FenceInst &I);
  void visitGetElementPtrInst(llvm::GetElementPtrInst &I);
  void visitVAArgInst(llvm::VAArgInst &I);

  void visitInsertElementInst(llvm::InsertElementInst &I);
  void visitExtractElementInst(llvm::ExtractElementInst &I);
  void visitShuffleVectorInst(llvm::ShuffleVectorInst &SVI);

  void visitInsertValueInst(llvm::InsertValueInst &I);
  void visitExtractValueInst(llvm::ExtractValueInst &I);

  void visitInstruction(llvm::Instruction &I) {
    CurInstr = &I;
    llvm_unreachable("unsupported LLVM instruction");
  }
  
  void outputLValue(llvm::Instruction *I) { Out << "  " << GetValueName(I) << " = "; }
  
  LLVM_ATTRIBUTE_NORETURN void errorWithMessage(const char *message);
  
  bool isGotoCodeNecessary(llvm::BasicBlock *From, llvm::BasicBlock *To);
  void printPHICopiesForSuccessor(llvm::BasicBlock *CurBlock, llvm::BasicBlock *Successor,
                                  unsigned Indent);
  void printBranchToBlock(llvm::BasicBlock *CurBlock, llvm::BasicBlock *SuccBlock,
                          unsigned Indent);
  void printGEPExpression(llvm::Value *Ptr, llvm::gep_type_iterator I, llvm::gep_type_iterator E);

  std::string GetValueName(llvm::Value *Operand);
};

}

#endif
