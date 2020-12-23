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
#include <set>

using namespace llvm;
using namespace c2ssa;

static std::string CBEMangle(const std::string &S) {
  std::string Result;

  for (auto c : S) {
    if (isalnum(c) || c == '_') {
      Result += c;
    } else {
      Result += '_';
      Result += 'A' + (c & 15);
      Result += 'A' + ((c >> 4) & 15);
      Result += '_';
    }
  }

  return Result;
}

// isFPCSafeToPrint - Returns true if we may assume that CFP may be written out
// textually as a double (rather than as a reference to a stack-allocated
// variable). We decide this by converting CFP to a string and back into a
// double, and then checking whether the conversion results in a bit-equal
// double to the original value of CFP. This depends on us and the target C
// compiler agreeing on the conversion process (which is pretty likely since we
// only deal in IEEE FP).

// TODO copied from CppBackend, new code should use raw_ostream
static inline std::string ftostr(const APFloat &V) {
  if (&V.getSemantics() != &APFloat::IEEEdouble() &&
      &V.getSemantics() != &APFloat::IEEEsingle()) {
    return "<unknown format in ftostr>"; // error
  }
  SmallVector<char, 32> Buffer;
  V.toString(Buffer);
  return std::string(Buffer.data(), Buffer.size());
}

static bool isFPCSafeToPrint(const ConstantFP *CFP) {
  bool ignored;
  // Do long doubles in hex for now.
  if (CFP->getType() != Type::getFloatTy(CFP->getContext()) &&
      CFP->getType() != Type::getDoubleTy(CFP->getContext()))
    return false;
  APFloat APF = APFloat(CFP->getValueAPF()); // copy
  if (CFP->getType() == Type::getFloatTy(CFP->getContext()))
    APF.convert(APFloat::IEEEdouble(), APFloat::rmNearestTiesToEven, &ignored);
#if HAVE_PRINTF_A && ENABLE_CBE_PRINTF_A
  char Buffer[100];
  sprintf(Buffer, "%a", APF.convertToDouble());
  if (!strncmp(Buffer, "0x", 2) || !strncmp(Buffer, "-0x", 3) ||
      !strncmp(Buffer, "+0x", 3))
    return APF.bitwiseIsEqual(APFloat(atof(Buffer)));
  return false;
#else
  std::string StrVal = ftostr(APF);

  while (StrVal[0] == ' ')
    StrVal.erase(StrVal.begin());

  // Check to make sure that the stringized number is not some string like "Inf"
  // or NaN.  Check that the string matches the "[-+]?[0-9]" regex.
  if ((StrVal[0] >= '0' && StrVal[0] <= '9') ||
      ((StrVal[0] == '-' || StrVal[0] == '+') &&
       (StrVal[1] >= '0' && StrVal[1] <= '9')))
    // Reparse stringized version!
    return APF.bitwiseIsEqual(APFloat(atof(StrVal.c_str())));
  return false;
#endif
}

static bool isEmptyType(Type *Ty) {
  if (StructType *STy = dyn_cast<StructType>(Ty))
    return STy->getNumElements() == 0 ||
           std::all_of(STy->element_begin(), STy->element_end(), isEmptyType);

  if (VectorType *VTy = dyn_cast<VectorType>(Ty))
    return VTy->getNumElements() == 0 || isEmptyType(VTy->getElementType());

  if (ArrayType *ATy = dyn_cast<ArrayType>(Ty))
    return ATy->getNumElements() == 0 || isEmptyType(ATy->getElementType());

  return Ty->isVoidTy();
}

static inline bool isFPIntBitCast(Instruction &I) {
  if (!isa<BitCastInst>(I))
    return false;
  Type *SrcTy = I.getOperand(0)->getType();
  Type *DstTy = I.getType();
  return (SrcTy->isFloatingPointTy() && DstTy->isIntegerTy()) ||
         (DstTy->isFloatingPointTy() && SrcTy->isIntegerTy());
}

class CWriter : public InstVisitor<CWriter> {
private:
  raw_ostream &Out;
  Function &F;
  
  LoopInfo *LI = nullptr;
  
  IDMap<const ConstantFP *> FPConstantMap;
  std::set<const Argument *> ByValParams;
  
  IDMap<const Value *> AnonValueNumbers;
  IDMap<std::pair<FunctionType *, std::pair<AttributeList, CallingConv::ID>>>
      UnnamedFunctionIDs;
  /// UnnamedStructIDs - This contains a unique ID for each struct that is
  /// either anonymous or has no name.
  IDMap<StructType *> UnnamedStructIDs;
  std::set<Type *> TypedefDeclTypes;
  
  unsigned LastAnnotatedSourceLine = 0;
  
  const Instruction *CurInstr = nullptr;
  
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
  
public:
  explicit CWriter(raw_ostream &Out, Function &F, LoopInfo *LI): Out(Out), F(F), LI(LI) {}
  
  void print() {
    /*
    if (!M.global_empty()) Out << '\n';
    for (const GlobalVariable &GV : M.globals()) {
      Out << "global var: " << GV.getName() << '\n';
    }
    */
    
    // Output all of the functions.
    /*
    for (Function &F : M) {
      Out << '\n';
      printFunction(F);
    }
    */
    printFunction(F);
  }
  
  void printFunction(Function &F) {
    bool isStructReturn = F.hasStructRetAttr();

    if (F.hasDLLImportStorageClass())
      Out << "__declspec(dllimport) ";
    if (F.hasDLLExportStorageClass())
      Out << "__declspec(dllexport) ";
    if (F.hasLocalLinkage())
      Out << "static ";
    
    iterator_range<Function::arg_iterator> args = F.args();
    printFunctionProto(Out, F.getFunctionType(),
                       std::make_pair(F.getAttributes(), F.getCallingConv()),
                       GetValueName(&F), &args);
    
    Out << " {\n";
    
    // If this is a struct return function, handle the result with magic.
    if (isStructReturn) {
      Type *StructTy =
          cast<PointerType>(F.arg_begin()->getType())->getElementType();
      Out << "  ";
      printTypeName(Out, StructTy, false)
          << " StructReturn;  /* Struct return temporary */\n";

      Out << "  ";
      printTypeName(Out, F.arg_begin()->getType(), false);
      Out << GetValueName(F.arg_begin()) << " = &StructReturn;\n";
    }
    
    bool PrintedVar = false;

    // print local variable information for the function
    for (inst_iterator I = inst_begin(&F), E = inst_end(&F); I != E; ++I) {
      if (AllocaInst *AI = isDirectAlloca(&*I)) {
        unsigned Alignment = AI->getAlignment();
        Out << "  ";
        printTypeName(Out, AI->getAllocatedType(), false) << ' ';
        Out << GetValueName(AI);
        Out << ";    /* Address-exposed local */\n";
        PrintedVar = true;
      } else if (!isEmptyType(I->getType()) && !isInlinableInst(*I)) {
        Out << "  ";
        printTypeName(Out, I->getType(), false) << ' ' << GetValueName(&*I);
        Out << ";\n";

        if (isa<PHINode>(*I)) { // Print out PHI node temporaries as well...
          Out << "  ";
          printTypeName(Out, I->getType(), false)
              << ' ' << (GetValueName(&*I) + "__PHI_TEMPORARY");
          Out << ";\n";
        }
        PrintedVar = true;
      }
      // We need a temporary for the BitCast to use so it can pluck a value out
      // of a union to do the BitCast. This is separate from the need for a
      // variable to hold the result of the BitCast.
      if (isFPIntBitCast(*I)) {
        // headerUseBitCastUnion();
        Out << "  llvmBitCastUnion " << GetValueName(&*I)
            << "__BITCAST_TEMPORARY;\n";
        PrintedVar = true;
      }
    }

    if (PrintedVar)
      Out << '\n';

    // print the basic blocks
    for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
      if (Loop *L = LI->getLoopFor(&*BB)) {
        if (L->getHeader() == &*BB && L->getParentLoop() == nullptr)
          printLoop(L);
      } else {
        printBasicBlock(&*BB);
      }
    }

    Out << "}\n\n";
  }
  
  void printLoop(Loop *L) {
    Out << "  do {     /* Syntactic loop '" << L->getHeader()->getName()
        << "' to make GCC happy */\n";
    for (unsigned i = 0, e = L->getBlocks().size(); i != e; ++i) {
      BasicBlock *BB = L->getBlocks()[i];
      Loop *BBLoop = LI->getLoopFor(BB);
      if (BBLoop == L)
        printBasicBlock(BB);
      else if (BB == BBLoop->getHeader() && BBLoop->getParentLoop() == L)
        printLoop(BBLoop);
    }
    Out << "  } while (1); /* end of syntactic loop '"
        << L->getHeader()->getName() << "' */\n";
  }
  
  void printBasicBlock(BasicBlock *BB) {
    // Don't print the label for the basic block if there are no uses, or if
    // the only terminator use is the predecessor basic block's terminator.
    // We have to scan the use list because PHI nodes use basic blocks too but
    // do not require a label to be generated.
    bool NeedsLabel = false;
    for (pred_iterator PI = pred_begin(BB), E = pred_end(BB); PI != E; ++PI)
      if (isGotoCodeNecessary(*PI, BB)) {
        NeedsLabel = true;
        break;
      }

    if (NeedsLabel)
      Out << GetValueName(BB) << ":\n";
    
    // Output all of the instructions in the basic block...
    for (BasicBlock::iterator II = BB->begin(), E = --BB->end(); II != E; ++II) {
      DILocation *Loc = (*II).getDebugLoc();
      if (Loc != nullptr && LastAnnotatedSourceLine != Loc->getLine()) {
        Out << "#line " << Loc->getLine() << " \"" << Loc->getDirectory() << "/" << Loc->getFilename() << "\"" << "\n";
        LastAnnotatedSourceLine = Loc->getLine();
      }
      if (!isInlinableInst(*II) && !isDirectAlloca(&*II)) {
        if (!isEmptyType(II->getType()) && !isInlineAsm(*II))
          outputLValue(&*II);

        else
          Out << "  ";
        writeInstComputationInline(*II);
        Out << ";\n";
      }
    }

    // Don't emit prefix or suffix for the terminator.
    visit(*BB->getTerminator());
  }
  
  void writeInstComputationInline(Instruction &I) {
    // C can't handle non-power-of-two integer types
    unsigned mask = 0;
    /*
    Type *Ty = I.getType();
    if (Ty->isIntegerTy()) {
      IntegerType *ITy = static_cast<IntegerType *>(Ty);
      if (!ITy->isPowerOf2ByteWidth())
        mask = ITy->getBitMask();
    }
    */

    // If this is a non-trivial bool computation, make sure to truncate down to
    // a 1 bit value.  This is important because we want "add i1 x, y" to return
    // "0" when x and y are true, not "2" for example.
    // Also truncate odd bit sizes
    if (mask)
      Out << "((";

    visit(I);

    if (mask)
      Out << ")&" << mask << ")";
  }
  
  bool isAddressExposed(Value *V) const {
    if (Argument *A = dyn_cast<Argument>(V))
      return ByValParams.count(A) > 0;
    else
      return isa<GlobalVariable>(V) || isDirectAlloca(V);
  }
  
  void writeOperand(Value *Operand, enum OperandContext Context = ContextNormal) {
    bool isAddressImplicit = isAddressExposed(Operand);
    if (isAddressImplicit)
      Out << "(&"; // Global variables are referenced as their addresses by llvm

    writeOperandInternal(Operand, Context);

    if (isAddressImplicit)
      Out << ')';
  }
  
  void writeOperandInternal(Value *Operand,
                            enum OperandContext Context) {
    if (Instruction *I = dyn_cast<Instruction>(Operand))
      // Should we inline this instruction to build a tree?
      if (isInlinableInst(*I) && !isDirectAlloca(I)) {
        Out << '(';
        writeInstComputationInline(*I);
        Out << ')';
        return;
      }

    Constant *CPV = dyn_cast<Constant>(Operand);

    if (CPV && !isa<GlobalValue>(CPV))
      printConstant(CPV, Context);
    else
      Out << GetValueName(Operand);
  }
  
  void printCast(unsigned opc, Type *SrcTy, Type *DstTy) {
    // Print the destination type cast
    switch (opc) {
    case Instruction::UIToFP:
    case Instruction::SIToFP:
    case Instruction::IntToPtr:
    case Instruction::Trunc:
    case Instruction::BitCast:
    case Instruction::FPExt:
    case Instruction::FPTrunc: // For these the DstTy sign doesn't matter
      Out << '(';
      printTypeName(Out, DstTy);
      Out << ')';
      break;
    case Instruction::ZExt:
    case Instruction::PtrToInt:
    case Instruction::FPToUI: // For these, make sure we get an unsigned dest
      Out << '(';
      printSimpleType(Out, DstTy, false);
      Out << ')';
      break;
    case Instruction::SExt:
    case Instruction::FPToSI: // For these, make sure we get a signed dest
      Out << '(';
      printSimpleType(Out, DstTy, true);
      Out << ')';
      break;
    default:
      llvm_unreachable("Invalid cast opcode");
    }

    // Print the source type cast
    switch (opc) {
    case Instruction::UIToFP:
    case Instruction::ZExt:
      Out << '(';
      printSimpleType(Out, SrcTy, false);
      Out << ')';
      break;
    case Instruction::SIToFP:
    case Instruction::SExt:
      Out << '(';
      printSimpleType(Out, SrcTy, true);
      Out << ')';
      break;
    case Instruction::IntToPtr:
    case Instruction::PtrToInt:
      // Avoid "cast to pointer from integer of different size" warnings
      Out << "(uintptr_t)";
      break;
    case Instruction::Trunc:
    case Instruction::BitCast:
    case Instruction::FPExt:
    case Instruction::FPTrunc:
    case Instruction::FPToSI:
    case Instruction::FPToUI:
      break; // These don't need a source cast.
    default:
      llvm_unreachable("Invalid cast opcode");
    }
  }
  
  void printGEPExpression(Value *Ptr, gep_type_iterator I,
                          gep_type_iterator E) {
    // If there are no indices, just print out the pointer.
    if (I == E) {
      writeOperand(Ptr);
      return;
    }

    // Find out if the last index is into a vector.  If so, we have to print this
    // specially.  Since vectors can't have elements of indexable type, only the
    // last index could possibly be of a vector element.
    VectorType *LastIndexIsVector = 0;
    {
      for (gep_type_iterator TmpI = I; TmpI != E; ++TmpI)
        LastIndexIsVector = dyn_cast<VectorType>(TmpI.getIndexedType());
    }

    Out << "(";

    // If the last index is into a vector, we can't print it as &a[i][j] because
    // we can't index into a vector with j in GCC.  Instead, emit this as
    // (((float*)&a[i])+j)
    // TODO: this is no longer true now that we don't represent vectors using
    // gcc-extentions
    if (LastIndexIsVector) {
      Out << "((";
      printTypeName(Out,
                    PointerType::getUnqual(LastIndexIsVector->getElementType()));
      Out << ")(";
    }

    Out << '&';

    Type *IntoT = I.getIndexedType();

    // If the first index is 0 (very typical) we can do a number of
    // simplifications to clean up the code.
    Value *FirstOp = I.getOperand();
    if (!isa<Constant>(FirstOp) || !cast<Constant>(FirstOp)->isNullValue()) {
      // First index isn't simple, print it the hard way.
      writeOperand(Ptr);
    } else {
      IntoT = I.getIndexedType();
      ++I; // Skip the zero index.

      // Okay, emit the first operand. If Ptr is something that is already address
      // exposed, like a global, avoid emitting (&foo)[0], just emit foo instead.
      if (isAddressExposed(Ptr)) {
        writeOperandInternal(Ptr);
      } else if (I != E && I.isStruct()) {
        // If we didn't already emit the first operand, see if we can print it as
        // P->f instead of "P[0].f"
        writeOperand(Ptr);
        Out << "->field" << cast<ConstantInt>(I.getOperand())->getZExtValue();
        IntoT = I.getIndexedType();
        ++I; // eat the struct index as well.
      } else {
        // Instead of emitting P[0][1], emit (*P)[1], which is more idiomatic.
        Out << "(*";
        writeOperand(Ptr);
        Out << ")";
      }
    }

    for (; I != E; ++I) {
      cwriter_assert(
          I.getOperand()
              ->getType()
              ->isIntegerTy()); // TODO: indexing a Vector with a Vector is valid,
                                // but we don't support it here
      if (I.isStruct()) {
        Out << ".field" << cast<ConstantInt>(I.getOperand())->getZExtValue();
      } else if (IntoT->isArrayTy()) {
        Out << ".array[";
        writeOperandWithCast(I.getOperand(), Instruction::GetElementPtr);
        Out << ']';
      } else if (!IntoT->isVectorTy()) {
        Out << '[';
        writeOperandWithCast(I.getOperand(), Instruction::GetElementPtr);
        Out << ']';
      } else {
        // If the last index is into a vector, then print it out as "+j)".  This
        // works with the 'LastIndexIsVector' code above.
        if (isa<Constant>(I.getOperand()) &&
            cast<Constant>(I.getOperand())->isNullValue()) {
          Out << "))"; // avoid "+0".
        } else {
          Out << ")+(";
          writeOperandWithCast(I.getOperand(), Instruction::GetElementPtr);
          Out << "))";
        }
      }

      IntoT = I.getIndexedType();
    }
    Out << ")";
  }
  
  void printConstant(Constant *CPV, enum OperandContext Context) {
    if (ConstantExpr *CE = dyn_cast<ConstantExpr>(CPV)) {
      // TODO: VectorType are valid here, but not supported
      if (!CE->getType()->isIntegerTy() && !CE->getType()->isFloatingPointTy() &&
          !CE->getType()->isPointerTy()) {
        Out << "Unsupported_constant_type";
        return;
      }
      switch (CE->getOpcode()) {
      case Instruction::Trunc:
      case Instruction::ZExt:
      case Instruction::SExt:
      case Instruction::FPTrunc:
      case Instruction::FPExt:
      case Instruction::UIToFP:
      case Instruction::SIToFP:
      case Instruction::FPToUI:
      case Instruction::FPToSI:
      case Instruction::PtrToInt:
      case Instruction::IntToPtr:
      case Instruction::BitCast:
        Out << "(";
        printCast(CE->getOpcode(), CE->getOperand(0)->getType(), CE->getType());
        if (CE->getOpcode() == Instruction::SExt &&
            CE->getOperand(0)->getType() == Type::getInt1Ty(CPV->getContext())) {
          // Make sure we really sext from bool here by subtracting from 0
          Out << "0-";
        }
        printConstant(CE->getOperand(0), ContextCasted);
        if (CE->getType() == Type::getInt1Ty(CPV->getContext()) &&
            (CE->getOpcode() == Instruction::Trunc ||
             CE->getOpcode() == Instruction::FPToUI ||
             CE->getOpcode() == Instruction::FPToSI ||
             CE->getOpcode() == Instruction::PtrToInt)) {
          // Make sure we really truncate to bool here by anding with 1
          Out << "&1u";
        }
        Out << ')';
        return;

      case Instruction::GetElementPtr:
        Out << "(";
        printGEPExpression(CE->getOperand(0), gep_type_begin(CPV),
                           gep_type_end(CPV));
        Out << ")";
        return;
      case Instruction::Select:
        Out << '(';
        printConstant(CE->getOperand(0), ContextCasted);
        Out << '?';
        printConstant(CE->getOperand(1), ContextNormal);
        Out << ':';
        printConstant(CE->getOperand(2), ContextNormal);
        Out << ')';
        return;
      case Instruction::Add:
      case Instruction::FAdd:
      case Instruction::Sub:
      case Instruction::FSub:
      case Instruction::Mul:
      case Instruction::FMul:
      case Instruction::SDiv:
      case Instruction::UDiv:
      case Instruction::FDiv:
      case Instruction::URem:
      case Instruction::SRem:
      case Instruction::FRem:
      case Instruction::And:
      case Instruction::Or:
      case Instruction::Xor:
      case Instruction::ICmp:
      case Instruction::Shl:
      case Instruction::LShr:
      case Instruction::AShr: {
        Out << '(';
        bool NeedsClosingParens = printConstExprCast(CE);
        printConstantWithCast(CE->getOperand(0), CE->getOpcode());
        switch (CE->getOpcode()) {
        case Instruction::Add:
        case Instruction::FAdd:
          Out << " + ";
          break;
        case Instruction::Sub:
        case Instruction::FSub:
          Out << " - ";
          break;
        case Instruction::Mul:
        case Instruction::FMul:
          Out << " * ";
          break;
        case Instruction::URem:
        case Instruction::SRem:
        case Instruction::FRem:
          Out << " % ";
          break;
        case Instruction::UDiv:
        case Instruction::SDiv:
        case Instruction::FDiv:
          Out << " / ";
          break;
        case Instruction::And:
          Out << " & ";
          break;
        case Instruction::Or:
          Out << " | ";
          break;
        case Instruction::Xor:
          Out << " ^ ";
          break;
        case Instruction::Shl:
          Out << " << ";
          break;
        case Instruction::LShr:
        case Instruction::AShr:
          Out << " >> ";
          break;
        case Instruction::ICmp:
          switch (CE->getPredicate()) {
          case ICmpInst::ICMP_EQ:
            Out << " == ";
            break;
          case ICmpInst::ICMP_NE:
            Out << " != ";
            break;
          case ICmpInst::ICMP_SLT:
          case ICmpInst::ICMP_ULT:
            Out << " < ";
            break;
          case ICmpInst::ICMP_SLE:
          case ICmpInst::ICMP_ULE:
            Out << " <= ";
            break;
          case ICmpInst::ICMP_SGT:
          case ICmpInst::ICMP_UGT:
            Out << " > ";
            break;
          case ICmpInst::ICMP_SGE:
          case ICmpInst::ICMP_UGE:
            Out << " >= ";
            break;
          default:
            llvm_unreachable("Illegal ICmp predicate");
          }
          break;
        default:
            llvm_unreachable("Illegal opcode here!");
        }
        printConstantWithCast(CE->getOperand(1), CE->getOpcode());
        if (NeedsClosingParens)
          Out << "))";
        Out << ')';
        return;
      }
      case Instruction::FCmp: {
        Out << '(';
        bool NeedsClosingParens = printConstExprCast(CE);
        if (CE->getPredicate() == FCmpInst::FCMP_FALSE)
          Out << "0";
        else if (CE->getPredicate() == FCmpInst::FCMP_TRUE)
          Out << "1";
        else {
          const auto Pred = (CmpInst::Predicate)CE->getPredicate();
          headerUseFCmpOp(Pred);
          Out << "llvm_fcmp_" << getCmpPredicateName(Pred) << "(";
          printConstant(CE->getOperand(0), ContextCasted);
          Out << ", ";
          printConstant(CE->getOperand(1), ContextCasted);
          Out << ")";
        }
        if (NeedsClosingParens)
          Out << "))";
        Out << ')';
        return;
      }
      default:
        llvm_unreachable("unhandled constant expression");
      }
    } else if (isa<UndefValue>(CPV) && CPV->getType()->isSingleValueType()) {
      Constant *Zero = Constant::getNullValue(CPV->getType());
      Out << "/*UNDEF*/";
      return printConstant(Zero, Context);
    }

    if (ConstantInt *CI = dyn_cast<ConstantInt>(CPV)) {
      Type *Ty = CI->getType();
      unsigned ActiveBits = CI->getValue().getMinSignedBits();
      if (Ty == Type::getInt1Ty(CPV->getContext())) {
        Out << (CI->getZExtValue() ? '1' : '0');
      } else if (Context != ContextNormal && ActiveBits < 64 &&
                 Ty->getPrimitiveSizeInBits() < 64 &&
                 ActiveBits < Ty->getPrimitiveSizeInBits()) {
        if (ActiveBits >= 32)
          Out << "INT64_C(";
        Out << CI->getSExtValue(); // most likely a shorter representation
        if (ActiveBits >= 32)
          Out << ")";
      } else if (Ty->getPrimitiveSizeInBits() < 32 && Context == ContextNormal) {
        Out << "((";
        printSimpleType(Out, Ty, false) << ')';
        if (CI->isMinValue(true))
          Out << CI->getZExtValue() << 'u';
        else
          Out << CI->getSExtValue();
        Out << ')';
      } else if (Ty->getPrimitiveSizeInBits() <= 32) {
        Out << CI->getZExtValue() << 'u';
      } else if (Ty->getPrimitiveSizeInBits() <= 64) {
        Out << "UINT64_C(" << CI->getZExtValue() << ")";
      } else if (Ty->getPrimitiveSizeInBits() <= 128) {
        // headerUseInt128();
        const APInt &V = CI->getValue();
        const APInt &Vlo = V.getLoBits(64);
        const APInt &Vhi = V.getHiBits(64);
        Out << (Context == ContextStatic ? "UINT128_C" : "llvm_ctor_u128");
        Out << "(UINT64_C(" << Vhi.getZExtValue() << "), UINT64_C("
            << Vlo.getZExtValue() << "))";
      }
      return;
    }

    switch (CPV->getType()->getTypeID()) {
    case Type::FloatTyID:
    case Type::DoubleTyID:
    case Type::X86_FP80TyID:
    case Type::PPC_FP128TyID:
    case Type::FP128TyID: {
      ConstantFP *FPC = cast<ConstantFP>(CPV);
      auto I = FPConstantMap.find(FPC);
      if (I != FPConstantMap.end()) {
        // Because of FP precision problems we must load from a stack allocated
        // value that holds the value in hex.
        Out << "(*("
            << (FPC->getType() == Type::getFloatTy(CPV->getContext())
                    ? "float"
                    : FPC->getType() == Type::getDoubleTy(CPV->getContext())
                          ? "double"
                          : "long double")
            << "*)&FPConstant" << I->second << ')';
      } else {
        double V;
        if (FPC->getType() == Type::getFloatTy(CPV->getContext()))
          V = FPC->getValueAPF().convertToFloat();
        else if (FPC->getType() == Type::getDoubleTy(CPV->getContext()))
          V = FPC->getValueAPF().convertToDouble();
        else {
          // Long double.  Convert the number to double, discarding precision.
          // This is not awesome, but it at least makes the CBE output somewhat
          // useful.
          APFloat Tmp = FPC->getValueAPF();
          bool LosesInfo;
          Tmp.convert(APFloat::IEEEdouble(), APFloat::rmTowardZero, &LosesInfo);
          V = Tmp.convertToDouble();
        }

        if (std::isnan(V)) {
          // The value is NaN

          // FIXME the actual NaN bits should be emitted.
          // The prefix for a quiet NaN is 0x7FF8. For a signalling NaN,
          // it's 0x7ff4.
          const unsigned long QuietNaN = 0x7ff8UL;
          // const unsigned long SignalNaN = 0x7ff4UL;

          // We need to grab the first part of the FP #
          char Buffer[100];

          uint64_t ll = DoubleToBits(V);
          sprintf(Buffer, "0x%llx", static_cast<long long>(ll));

          std::string Num(&Buffer[0], &Buffer[6]);
          unsigned long Val = strtoul(Num.c_str(), 0, 16);

          // headerUseNanInf();
          if (FPC->getType() == Type::getFloatTy(FPC->getContext()))
            Out << "LLVM_NAN" << (Val == QuietNaN ? "" : "S") << "F(\"" << Buffer
                << "\") /*nan*/ ";
          else
            Out << "LLVM_NAN" << (Val == QuietNaN ? "" : "S") << "(\"" << Buffer
                << "\") /*nan*/ ";
        } else if (std::isinf(V)) {
          // The value is Inf
          if (V < 0)
            Out << '-';
          // headerUseNanInf();
          Out << "LLVM_INF"
              << (FPC->getType() == Type::getFloatTy(FPC->getContext()) ? "F"
                                                                        : "")
              << " /*inf*/ ";
        } else {
          std::string Num;
  #if HAVE_PRINTF_A && ENABLE_CBE_PRINTF_A
          // Print out the constant as a floating point number.
          char Buffer[100];
          sprintf(Buffer, "%a", V);
          Num = Buffer;
  #else
          Num = ftostr(FPC->getValueAPF());
  #endif
          Out << Num;
        }
      }
      break;
    }

    case Type::ArrayTyID: {
      if (printConstantString(CPV, Context))
        break;
      ArrayType *AT = cast<ArrayType>(CPV->getType());
      cwriter_assert(AT->getNumElements() != 0 && !isEmptyType(AT));
      if (Context != ContextStatic) {
        CtorDeclTypes.insert(AT);
        Out << "llvm_ctor_";
        printTypeString(Out, AT, false);
        Out << "(";
        Context = ContextCasted;
      } else {
        Out << "{ { "; // Arrays are wrapped in struct types.
      }
      if (ConstantArray *CA = dyn_cast<ConstantArray>(CPV)) {
        printConstantArray(CA, Context);
      } else if (ConstantDataSequential *CDS =
                     dyn_cast<ConstantDataSequential>(CPV)) {
        printConstantDataSequential(CDS, Context);
      } else {
        cwriter_assert(isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV));
        Constant *CZ = Constant::getNullValue(AT->getElementType());
        printConstant(CZ, Context);
        for (unsigned i = 1, e = AT->getNumElements(); i != e; ++i) {
          Out << ", ";
          printConstant(CZ, Context);
        }
      }
      Out << (Context == ContextStatic
                  ? " } }"
                  : ")"); // Arrays are wrapped in struct types.
      break;
    }

    case Type::VectorTyID: {
      VectorType *VT = cast<VectorType>(CPV->getType());
      cwriter_assert(VT->getNumElements() != 0 && !isEmptyType(VT));
      if (Context != ContextStatic) {
        CtorDeclTypes.insert(VT);
        Out << "llvm_ctor_";
        printTypeString(Out, VT, false);
        Out << "(";
        Context = ContextCasted;
      } else {
        Out << "{ ";
      }
      if (ConstantVector *CV = dyn_cast<ConstantVector>(CPV)) {
        printConstantVector(CV, Context);
      } else if (ConstantDataSequential *CDS =
                     dyn_cast<ConstantDataSequential>(CPV)) {
        printConstantDataSequential(CDS, Context);
      } else {
        cwriter_assert(isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV));
        Constant *CZ = Constant::getNullValue(VT->getElementType());
        printConstant(CZ, Context);
        for (unsigned i = 1, e = VT->getNumElements(); i != e; ++i) {
          Out << ", ";
          printConstant(CZ, Context);
        }
      }
      Out << (Context == ContextStatic ? " }" : ")");
      break;
    }

    case Type::StructTyID: {
      StructType *ST = cast<StructType>(CPV->getType());
      cwriter_assert(!isEmptyType(ST));
      if (Context != ContextStatic) {
        CtorDeclTypes.insert(ST);
        Out << "llvm_ctor_";
        printTypeString(Out, ST, false);
        Out << "(";
        Context = ContextCasted;
      } else {
        Out << "{ ";
      }

      if (isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV)) {
        bool printed = false;
        for (unsigned i = 0, e = ST->getNumElements(); i != e; ++i) {
          Type *ElTy = ST->getElementType(i);
          if (isEmptyType(ElTy))
            continue;
          if (printed)
            Out << ", ";
          printConstant(Constant::getNullValue(ElTy), Context);
          printed = true;
        }
        cwriter_assert(printed);
      } else {
        bool printed = false;
        for (unsigned i = 0, e = CPV->getNumOperands(); i != e; ++i) {
          Constant *C = cast<Constant>(CPV->getOperand(i));
          if (isEmptyType(C->getType()))
            continue;
          if (printed)
            Out << ", ";
          printConstant(C, Context);
          printed = true;
        }
        cwriter_assert(printed);
      }
      Out << (Context == ContextStatic ? " }" : ")");
      break;
    }

    case Type::PointerTyID:
      if (isa<ConstantPointerNull>(CPV)) {
        Out << "((";
        printTypeName(Out, CPV->getType()); // sign doesn't matter
        Out << ")/*NULL*/0)";
        break;
      } else if (GlobalValue *GV = dyn_cast<GlobalValue>(CPV)) {
        writeOperand(GV);
        break;
      }
      // FALL THROUGH
    default:
      llvm_unreachable("unknown constant type");
    }
  }
  
  void visitReturnInst(ReturnInst &I) {
    CurInstr = &I;

    // If this is a struct return function, return the temporary struct.
    bool isStructReturn = I.getParent()->getParent()->hasStructRetAttr();

    if (isStructReturn) {
      Out << "  return StructReturn;\n";
      return;
    }

    // Don't output a void return if this is the last basic block in the function
    // unless that would make the basic block empty
    if (I.getNumOperands() == 0 &&
        &*--I.getParent()->getParent()->end() == I.getParent() &&
        &*I.getParent()->begin() != &I) {
      return;
    }

    Out << "  return";
    if (I.getNumOperands()) {
      Out << ' ';
      writeOperand(I.getOperand(0), ContextCasted);
    }
    Out << ";\n";
  }
  
  void outputLValue(Instruction *I) { Out << "  " << GetValueName(I) << " = "; }
  
  bool isInlineAsm(Instruction &I) const {
    /// TODO:
    return false;
  }
  
  bool isGotoCodeNecessary(BasicBlock *From, BasicBlock *To) const {
    /// TODO:
    return true;
  }
  
  bool isInlinableInst(Instruction &I) const {
    // Always inline cmp instructions, even if they are shared by multiple
    // expressions.  GCC generates horrible code if we don't.
    if (isa<CmpInst>(I))
      return true;

    // Must be an expression, must be used exactly once.  If it is dead, we
    // emit it inline where it would go.
    if (isEmptyType(I.getType()) || !I.hasOneUse() || I.isTerminator() ||
        isa<CallInst>(I) || isa<PHINode>(I) || isa<LoadInst>(I) ||
        isa<VAArgInst>(I) || isa<InsertElementInst>(I) || isa<InsertValueInst>(I))
      // Don't inline a load across a store or other bad things!
      return false;

    // Only inline instruction if its use is in the same BB as the inst.
    return I.getParent() == cast<Instruction>(I.user_back())->getParent();
  }
  
  AllocaInst *isDirectAlloca(Value *V) const {
    AllocaInst *AI = dyn_cast<AllocaInst>(V);
    if (!AI)
      return nullptr;
    if (AI->isArrayAllocation())
      return nullptr; // FIXME: we can also inline fixed size array allocas!
    if (AI->getParent() != &AI->getParent()->getParent()->getEntryBlock())
      return nullptr;
    return AI;
  }
  
  void printFunctionProto(raw_ostream &Out, FunctionType *FTy,
                          std::pair<AttributeList, CallingConv::ID> Attrs,
                          const std::string &Name,
                          iterator_range<Function::arg_iterator> *ArgList) {
    AttributeList &PAL = Attrs.first;

    // Should this function actually return a struct by-value?
    bool isStructReturn = PAL.hasAttribute(1, Attribute::StructRet) ||
                          PAL.hasAttribute(2, Attribute::StructRet);
    // Get the return type for the function.
    Type *RetTy;
    if (!isStructReturn)
      RetTy = FTy->getReturnType();
    else {
      // If this is a struct-return function, print the struct-return type.
      RetTy = cast<PointerType>(FTy->getParamType(0))->getElementType();
    }
    printTypeName(Out, RetTy,
                  /*isSigned=*/
                  PAL.hasAttribute(AttributeList::ReturnIndex, Attribute::SExt));

    switch (Attrs.second) {
    case CallingConv::C:
      break;
    // Consider the LLVM fast calling convention as the same as the C calling convention for now.
    case CallingConv::Fast:
      break;
    case CallingConv::X86_StdCall:
      Out << " __stdcall";
      break;
    case CallingConv::X86_FastCall:
      Out << " __fastcall";
      break;
    case CallingConv::X86_ThisCall:
      Out << " __thiscall";
      break;
    default:
      Out << " !!!!!!!!!!!!!!!!!!!!!!!!!";
      break;
    }
    Out << ' ' << Name << '(';

    unsigned Idx = 1;
    bool PrintedArg = false;
    FunctionType::param_iterator I = FTy->param_begin(), E = FTy->param_end();
    Function::arg_iterator ArgName =
        ArgList ? ArgList->begin() : Function::arg_iterator();

    // If this is a struct-return function, don't print the hidden
    // struct-return argument.
    if (isStructReturn) {
      ++I;
      ++Idx;
      if (ArgList)
        ++ArgName;
    }

    for (; I != E; ++I) {
      Type *ArgTy = *I;
      if (PAL.hasAttribute(Idx, Attribute::ByVal)) {
        ArgTy = cast<PointerType>(ArgTy)->getElementType();
      }
      if (PrintedArg)
        Out << ", ";
      printTypeNameUnaligned(Out, ArgTy,
                             /*isSigned=*/PAL.hasAttribute(Idx, Attribute::SExt));
      PrintedArg = true;
      ++Idx;
      if (ArgList) {
        Out << ' ' << GetValueName(ArgName);
        ++ArgName;
      }
    }

    if (FTy->isVarArg()) {
      if (!PrintedArg) {
        Out << "int"; // dummy argument for empty vaarg functs
        if (ArgList)
          Out << " vararg_dummy_arg";
      }
      Out << ", ...";
    } else if (!PrintedArg) {
      Out << "void";
    }
    Out << ")";
  }
  
  raw_ostream& printTypeNameUnaligned(raw_ostream &Out, Type *Ty,
                                      bool isSigned) {
    return printTypeName(Out, Ty, isSigned);
  }
  
  raw_ostream& printSimpleType(raw_ostream &Out, Type *Ty,
                                bool isSigned) {
    switch (Ty->getTypeID()) {
    case Type::VoidTyID:
      return Out << "void";
    case Type::IntegerTyID: {
      unsigned NumBits = cast<IntegerType>(Ty)->getBitWidth();
      if (NumBits == 1)
        return Out << "bool";
      else if (NumBits <= 8)
        return Out << (isSigned ? "int8_t" : "uint8_t");
      else if (NumBits <= 16)
        return Out << (isSigned ? "int16_t" : "uint16_t");
      else if (NumBits <= 32)
        return Out << (isSigned ? "int32_t" : "uint32_t");
      else if (NumBits <= 64)
        return Out << (isSigned ? "int64_t" : "uint64_t");
      else {
        return Out << (isSigned ? "int128_t" : "uint128_t");
      }
    }
    case Type::FloatTyID:
      return Out << "float";
    case Type::DoubleTyID:
      return Out << "double";
    // Lacking emulation of FP80 on PPC, etc., we assume whichever of these is
    // present matches host 'long double'.
    case Type::X86_FP80TyID:
    case Type::PPC_FP128TyID:
    case Type::FP128TyID:
      return Out << "long double";

    case Type::X86_MMXTyID:
      return Out << (isSigned ? "int32_t" : "uint32_t")
                 << " __attribute__((vector_size(8)))";

    default:
        return Out << "unkown_type";
    }
  }
  
  raw_ostream& printTypeName(raw_ostream &Out, Type *Ty, bool isSigned,
                     std::pair<AttributeList, CallingConv::ID> PAL = std::make_pair(AttributeList(),
                                                                                    CallingConv::C)) {
    if (Ty->isSingleValueType() || Ty->isVoidTy()) {
      if (!Ty->isPointerTy() && !Ty->isVectorTy())
        return printSimpleType(Out, Ty, isSigned);
    }

    if (isEmptyType(Ty))
      return Out << "void";

    switch (Ty->getTypeID()) {
    case Type::FunctionTyID: {
      FunctionType *FTy = cast<FunctionType>(Ty);
      return Out << getFunctionName(FTy, PAL);
    }
    case Type::StructTyID: {
      TypedefDeclTypes.insert(Ty);
      return Out << getStructName(cast<StructType>(Ty));
    }

    case Type::PointerTyID: {
      Type *ElTy = Ty->getPointerElementType();
      return printTypeName(Out, ElTy, false) << '*';
    }

    case Type::ArrayTyID: {
      TypedefDeclTypes.insert(Ty);
      return Out << getArrayName(cast<ArrayType>(Ty));
    }

    default:
        return Out << "unexpected_type";
    }
  }
  
  std::string getArrayName(ArrayType *AT) {
    std::string astr;
    raw_string_ostream ArrayInnards(astr);
    // Arrays are wrapped in structs to allow them to have normal
    // value semantics (avoiding the array "decay").
    printTypeName(ArrayInnards, AT->getElementType(), false);
    return "struct l_array_" + utostr(AT->getNumElements()) + '_' +
           CBEMangle(ArrayInnards.str());
  }
  
  std::string getStructName(StructType *ST) {
    if (!ST->isLiteral() && !ST->getName().empty())
      return "struct l_struct_" + CBEMangle(ST->getName().str());

    unsigned id = UnnamedStructIDs.getOrInsert(ST);
    return "struct l_unnamed_" + utostr(id);
  }
  
  std::string getFunctionName(FunctionType *FT,
                              std::pair<AttributeList, CallingConv::ID> PAL) {
    unsigned id = UnnamedFunctionIDs.getOrInsert(std::make_pair(FT, PAL));
    return "l_fptr_" + utostr(id);
  }
  
  std::string GetValueName(Value *Operand) {
    // Resolve potential alias.
    if (GlobalAlias *GA = dyn_cast<GlobalAlias>(Operand)) {
      Operand = GA->getAliasee();
    }

    std::string Name = Operand->getName().str();
    if (Name.empty()) { // Assign unique names to local temporaries.
      unsigned No = AnonValueNumbers.getOrInsert(Operand);
      Name = "tmp__" + utostr(No);
    }

    // Mangle globals with the standard mangler interface for LLC compatibility.
    if (isa<GlobalValue>(Operand)) {
      return CBEMangle(Name);
    }

    std::string VarName;
    VarName.reserve(Name.capacity());

    for (std::string::iterator I = Name.begin(), E = Name.end(); I != E; ++I) {
      unsigned char ch = *I;

      if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
            (ch >= '0' && ch <= '9') || ch == '_')) {
        char buffer[5];
        sprintf(buffer, "_%x_", ch);
        VarName += buffer;
      } else
        VarName += ch;
    }

    return "c2ssa_" + VarName;
  }
};

PrintModulePass::PrintModulePass() : OS(dbgs()) {}
PrintModulePass::PrintModulePass(raw_ostream &OS, const std::string &Banner,
                                 bool ShouldPreserveUseListOrder)
    : OS(OS), Banner(Banner),
      ShouldPreserveUseListOrder(ShouldPreserveUseListOrder) {}

PreservedAnalyses PrintModulePass::run(Function &F, FunctionAnalysisManager &AM) {
  if (!Banner.empty())
    OS << Banner << "\n";

  auto &LI = AM.getResult<LoopAnalysis>(F);

  CWriter writer(OS, F, &LI);
  writer.print();

  return PreservedAnalyses::all();
}
