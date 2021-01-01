#include "CodeGenAction.h"
#include "BackendUtil.h"
#include "CodeGen/CodeGenModule.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclGroup.h"
#include "clang/Basic/DiagnosticFrontend.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/LangStandard.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/Driver/DriverDiagnostic.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendDiagnostic.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/CodeGen/MachineOptimizationRemarkEmitter.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LLVMRemarkStreamer.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/LTO/LTOBackend.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Pass.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TimeProfiler.h"
#include "llvm/Support/Timer.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Transforms/IPO/Internalize.h"

#include <memory>
using namespace clang;
using namespace llvm;
using namespace c2ssa;

namespace c2ssa {
  class BackendConsumer;
  class ClangDiagnosticHandler final : public DiagnosticHandler {
  public:
    ClangDiagnosticHandler(const CodeGenOptions &CGOpts, BackendConsumer *BCon)
        : CodeGenOpts(CGOpts), BackendCon(BCon) {}

    bool handleDiagnostics(const DiagnosticInfo &DI) override;

    bool isAnalysisRemarkEnabled(StringRef PassName) const override {
      return (CodeGenOpts.OptimizationRemarkAnalysisPattern &&
              CodeGenOpts.OptimizationRemarkAnalysisPattern->match(PassName));
    }
    bool isMissedOptRemarkEnabled(StringRef PassName) const override {
      return (CodeGenOpts.OptimizationRemarkMissedPattern &&
              CodeGenOpts.OptimizationRemarkMissedPattern->match(PassName));
    }
    bool isPassedOptRemarkEnabled(StringRef PassName) const override {
      return (CodeGenOpts.OptimizationRemarkPattern &&
              CodeGenOpts.OptimizationRemarkPattern->match(PassName));
    }

    bool isAnyRemarkEnabled() const override {
      return (CodeGenOpts.OptimizationRemarkAnalysisPattern ||
              CodeGenOpts.OptimizationRemarkMissedPattern ||
              CodeGenOpts.OptimizationRemarkPattern);
    }

  private:
    const CodeGenOptions &CodeGenOpts;
    BackendConsumer *BackendCon;
  };

  static void reportOptRecordError(Error E, DiagnosticsEngine &Diags,
                                   const CodeGenOptions CodeGenOpts) {
    handleAllErrors(
        std::move(E),
      [&](const LLVMRemarkSetupFileError &E) {
          Diags.Report(diag::err_cannot_open_file)
              << CodeGenOpts.OptRecordFile << E.message();
        },
      [&](const LLVMRemarkSetupPatternError &E) {
          Diags.Report(diag::err_drv_optimization_remark_pattern)
              << E.message() << CodeGenOpts.OptRecordPasses;
        },
      [&](const LLVMRemarkSetupFormatError &E) {
          Diags.Report(diag::err_drv_optimization_remark_format)
              << CodeGenOpts.OptRecordFormat;
        });
    }

  class BackendConsumer : public ASTConsumer {
    DiagnosticsEngine &Diags;
    const HeaderSearchOptions &HeaderSearchOpts;
    const CodeGenOptions &CodeGenOpts;
    const clang::TargetOptions &TargetOpts;
    const LangOptions &LangOpts;
    std::unique_ptr<raw_pwrite_stream> AsmOutStream;
    ASTContext *Context;

    Timer LLVMIRGeneration;
    unsigned LLVMIRGenerationRefCount;

    /// True if we've finished generating IR. This prevents us from generating
    /// additional LLVM IR after emitting output in HandleTranslationUnit. This
    /// can happen when Clang plugins trigger additional AST deserialization.
    bool IRGenFinished = false;

    bool TimerIsEnabled = false;

    std::unique_ptr<CodeGenerator> Gen;

    // This is here so that the diagnostic printer knows the module a diagnostic
    // refers to.
    llvm::Module *CurLinkModule = nullptr;

  public:
    BackendConsumer(DiagnosticsEngine &Diags,
                    const HeaderSearchOptions &HeaderSearchOpts,
                    const PreprocessorOptions &PPOpts,
                    const CodeGenOptions &CodeGenOpts,
                    const clang::TargetOptions &TargetOpts,
                    const LangOptions &LangOpts, const std::string &InFile,
                    std::unique_ptr<raw_pwrite_stream> OS, LLVMContext &C,
                    CoverageSourceInfo *CoverageInfo = nullptr)
        : Diags(Diags), HeaderSearchOpts(HeaderSearchOpts),
          CodeGenOpts(CodeGenOpts), TargetOpts(TargetOpts), LangOpts(LangOpts),
          AsmOutStream(std::move(OS)), Context(nullptr),
          LLVMIRGeneration("irgen", "LLVM IR Generation Time"),
          LLVMIRGenerationRefCount(0),
          Gen(CreateLLVMCodeGen(Diags, InFile, HeaderSearchOpts, PPOpts,
                                CodeGenOpts, C, CoverageInfo)) {
      TimerIsEnabled = CodeGenOpts.TimePasses;
      llvm::TimePassesIsEnabled = CodeGenOpts.TimePasses;
      llvm::TimePassesPerRun = CodeGenOpts.TimePassesPerRun;
    }

    llvm::Module *getModule() const { return Gen->GetModule(); }
    std::unique_ptr<llvm::Module> takeModule() {
      return std::unique_ptr<llvm::Module>(Gen->ReleaseModule());
    }

    CodeGenerator *getCodeGenerator() { return Gen.get(); }

    void Initialize(ASTContext &Ctx) override {
      assert(!Context && "initialized multiple times");

      Context = &Ctx;

      if (TimerIsEnabled)
        LLVMIRGeneration.startTimer();

      Gen->Initialize(Ctx);

      if (TimerIsEnabled)
        LLVMIRGeneration.stopTimer();
    }

    bool HandleTopLevelDecl(DeclGroupRef D) override {
      PrettyStackTraceDecl CrashInfo(*D.begin(), SourceLocation(),
                                     Context->getSourceManager(),
                                     "Generation of declaration");

      // Recurse.
      if (TimerIsEnabled) {
        LLVMIRGenerationRefCount += 1;
        if (LLVMIRGenerationRefCount == 1)
          LLVMIRGeneration.startTimer();
      }

      Gen->HandleTopLevelDecl(D);

      if (TimerIsEnabled) {
        LLVMIRGenerationRefCount -= 1;
        if (LLVMIRGenerationRefCount == 0)
          LLVMIRGeneration.stopTimer();
      }

      return true;
    }

    void HandleInlineFunctionDefinition(FunctionDecl *D) override {
      PrettyStackTraceDecl CrashInfo(D, SourceLocation(),
                                     Context->getSourceManager(),
                                     "Generation of inline function");
      if (TimerIsEnabled)
        LLVMIRGeneration.startTimer();

      Gen->HandleInlineFunctionDefinition(D);

      if (TimerIsEnabled)
        LLVMIRGeneration.stopTimer();
    }

    void HandleInterestingDecl(DeclGroupRef D) override {
      // Ignore interesting decls from the AST reader after IRGen is finished.
      if (!IRGenFinished)
        HandleTopLevelDecl(D);
    }

    void HandleTranslationUnit(ASTContext &C) override {
      {
        llvm::TimeTraceScope TimeScope("Frontend");
        PrettyStackTraceString CrashInfo("Per-file LLVM IR generation");
        if (TimerIsEnabled) {
          LLVMIRGenerationRefCount += 1;
          if (LLVMIRGenerationRefCount == 1)
            LLVMIRGeneration.startTimer();
        }

        Gen->HandleTranslationUnit(C);

        if (TimerIsEnabled) {
          LLVMIRGenerationRefCount -= 1;
          if (LLVMIRGenerationRefCount == 0)
            LLVMIRGeneration.stopTimer();
        }

        IRGenFinished = true;
      }

      // Silently ignore if we weren't initialized for some reason.
      if (!getModule())
        return;

      // Install an inline asm handler so that diagnostics get printed through
      // our diagnostics hooks.
      LLVMContext &Ctx = getModule()->getContext();
      LLVMContext::InlineAsmDiagHandlerTy OldHandler =
        Ctx.getInlineAsmDiagnosticHandler();
      void *OldContext = Ctx.getInlineAsmDiagnosticContext();
      Ctx.setInlineAsmDiagnosticHandler(InlineAsmDiagHandler, this);

      std::unique_ptr<DiagnosticHandler> OldDiagnosticHandler =
          Ctx.getDiagnosticHandler();
      Ctx.setDiagnosticHandler(std::make_unique<ClangDiagnosticHandler>(
        CodeGenOpts, this));

      Expected<std::unique_ptr<llvm::ToolOutputFile>> OptRecordFileOrErr =
          setupLLVMOptimizationRemarks(
              Ctx, CodeGenOpts.OptRecordFile, CodeGenOpts.OptRecordPasses,
              CodeGenOpts.OptRecordFormat, CodeGenOpts.DiagnosticsWithHotness,
              CodeGenOpts.DiagnosticsHotnessThreshold);

      if (Error E = OptRecordFileOrErr.takeError()) {
        reportOptRecordError(std::move(E), Diags, CodeGenOpts);
        return;
      }

      std::unique_ptr<llvm::ToolOutputFile> OptRecordFile =
          std::move(*OptRecordFileOrErr);

      if (OptRecordFile &&
          CodeGenOpts.getProfileUse() != CodeGenOptions::ProfileNone)
        Ctx.setDiagnosticsHotnessRequested(true);

      EmitBackendOutput(Diags, HeaderSearchOpts, CodeGenOpts, TargetOpts,
                        LangOpts, C.getTargetInfo().getDataLayout(),
                        getModule(), std::move(AsmOutStream));

      Ctx.setInlineAsmDiagnosticHandler(OldHandler, OldContext);

      Ctx.setDiagnosticHandler(std::move(OldDiagnosticHandler));

      if (OptRecordFile)
        OptRecordFile->keep();
    }

    void HandleTagDeclDefinition(TagDecl *D) override {
      PrettyStackTraceDecl CrashInfo(D, SourceLocation(),
                                     Context->getSourceManager(),
                                     "LLVM IR generation of declaration");
      Gen->HandleTagDeclDefinition(D);
    }

    void HandleTagDeclRequiredDefinition(const TagDecl *D) override {
      Gen->HandleTagDeclRequiredDefinition(D);
    }

    void CompleteTentativeDefinition(VarDecl *D) override {
      Gen->CompleteTentativeDefinition(D);
    }

    void CompleteExternalDeclaration(VarDecl *D) override {
      Gen->CompleteExternalDeclaration(D);
    }

    void AssignInheritanceModel(CXXRecordDecl *RD) override {
      Gen->AssignInheritanceModel(RD);
    }

    void HandleVTable(CXXRecordDecl *RD) override {
      Gen->HandleVTable(RD);
    }

    static void InlineAsmDiagHandler(const llvm::SMDiagnostic &SM,void *Context,
                                     unsigned LocCookie) {
      SourceLocation Loc = SourceLocation::getFromRawEncoding(LocCookie);
      ((BackendConsumer*)Context)->InlineAsmDiagHandler2(SM, Loc);
    }

    /// Get the best possible source location to represent a diagnostic that
    /// may have associated debug info.
    const FullSourceLoc
    getBestLocationFromDebugLoc(const llvm::DiagnosticInfoWithLocationBase &D,
                                bool &BadDebugInfo, StringRef &Filename,
                                unsigned &Line, unsigned &Column) const;

    void InlineAsmDiagHandler2(const llvm::SMDiagnostic &,
                               SourceLocation LocCookie);

    void DiagnosticHandlerImpl(const llvm::DiagnosticInfo &DI);
    /// Specialized handler for InlineAsm diagnostic.
    /// \return True if the diagnostic has been successfully reported, false
    /// otherwise.
    bool InlineAsmDiagHandler(const llvm::DiagnosticInfoInlineAsm &D);
    /// Specialized handler for StackSize diagnostic.
    /// \return True if the diagnostic has been successfully reported, false
    /// otherwise.
    bool StackSizeDiagHandler(const llvm::DiagnosticInfoStackSize &D);
    /// Specialized handler for unsupported backend feature diagnostic.
    void UnsupportedDiagHandler(const llvm::DiagnosticInfoUnsupported &D);
    /// Specialized handlers for optimization remarks.
    /// Note that these handlers only accept remarks and they always handle
    /// them.
    void EmitOptimizationMessage(const llvm::DiagnosticInfoOptimizationBase &D,
                                 unsigned DiagID);
    void
    OptimizationRemarkHandler(const llvm::DiagnosticInfoOptimizationBase &D);
    void OptimizationRemarkHandler(
        const llvm::OptimizationRemarkAnalysisFPCommute &D);
    void OptimizationRemarkHandler(
        const llvm::OptimizationRemarkAnalysisAliasing &D);
    void OptimizationFailureHandler(
        const llvm::DiagnosticInfoOptimizationFailure &D);
  };
}

bool ClangDiagnosticHandler::handleDiagnostics(const DiagnosticInfo &DI) {
  BackendCon->DiagnosticHandlerImpl(DI);
  return true;
}

/// ConvertBackendLocation - Convert a location in a temporary llvm::SourceMgr
/// buffer to be a valid FullSourceLoc.
static FullSourceLoc ConvertBackendLocation(const llvm::SMDiagnostic &D,
                                            SourceManager &CSM) {
  // Get both the clang and llvm source managers.  The location is relative to
  // a memory buffer that the LLVM Source Manager is handling, we need to add
  // a copy to the Clang source manager.
  const llvm::SourceMgr &LSM = *D.getSourceMgr();

  // We need to copy the underlying LLVM memory buffer because llvm::SourceMgr
  // already owns its one and clang::SourceManager wants to own its one.
  const MemoryBuffer *LBuf =
  LSM.getMemoryBuffer(LSM.FindBufferContainingLoc(D.getLoc()));

  // Create the copy and transfer ownership to clang::SourceManager.
  // TODO: Avoid copying files into memory.
  std::unique_ptr<llvm::MemoryBuffer> CBuf =
      llvm::MemoryBuffer::getMemBufferCopy(LBuf->getBuffer(),
                                           LBuf->getBufferIdentifier());
  // FIXME: Keep a file ID map instead of creating new IDs for each location.
  FileID FID = CSM.createFileID(std::move(CBuf));

  // Translate the offset into the file.
  unsigned Offset = D.getLoc().getPointer() - LBuf->getBufferStart();
  SourceLocation NewLoc =
  CSM.getLocForStartOfFile(FID).getLocWithOffset(Offset);
  return FullSourceLoc(NewLoc, CSM);
}


/// InlineAsmDiagHandler2 - This function is invoked when the backend hits an
/// error parsing inline asm.  The SMDiagnostic indicates the error relative to
/// the temporary memory buffer that the inline asm parser has set up.
void BackendConsumer::InlineAsmDiagHandler2(const llvm::SMDiagnostic &D,
                                            SourceLocation LocCookie) {
  // There are a couple of different kinds of errors we could get here.  First,
  // we re-format the SMDiagnostic in terms of a clang diagnostic.

  // Strip "error: " off the start of the message string.
  StringRef Message = D.getMessage();
  if (Message.startswith("error: "))
    Message = Message.substr(7);

  // If the SMDiagnostic has an inline asm source location, translate it.
  FullSourceLoc Loc;
  if (D.getLoc() != SMLoc())
    Loc = ConvertBackendLocation(D, Context->getSourceManager());

  unsigned DiagID;
  switch (D.getKind()) {
  case llvm::SourceMgr::DK_Error:
    DiagID = diag::err_fe_inline_asm;
    break;
  case llvm::SourceMgr::DK_Warning:
    DiagID = diag::warn_fe_inline_asm;
    break;
  case llvm::SourceMgr::DK_Note:
    DiagID = diag::note_fe_inline_asm;
    break;
  case llvm::SourceMgr::DK_Remark:
    llvm_unreachable("remarks unexpected");
  }
  // If this problem has clang-level source location information, report the
  // issue in the source with a note showing the instantiated
  // code.
  if (LocCookie.isValid()) {
    Diags.Report(LocCookie, DiagID).AddString(Message);

    if (D.getLoc().isValid()) {
      DiagnosticBuilder B = Diags.Report(Loc, diag::note_fe_inline_asm_here);
      // Convert the SMDiagnostic ranges into SourceRange and attach them
      // to the diagnostic.
      for (const std::pair<unsigned, unsigned> &Range : D.getRanges()) {
        unsigned Column = D.getColumnNo();
        B << SourceRange(Loc.getLocWithOffset(Range.first - Column),
                         Loc.getLocWithOffset(Range.second - Column));
      }
    }
    return;
  }

  // Otherwise, report the backend issue as occurring in the generated .s file.
  // If Loc is invalid, we still need to report the issue, it just gets no
  // location info.
  Diags.Report(Loc, DiagID).AddString(Message);
}

#define ComputeDiagID(Severity, GroupName, DiagID)                             \
  do {                                                                         \
    switch (Severity) {                                                        \
    case llvm::DS_Error:                                                       \
      DiagID = diag::err_fe_##GroupName;                                       \
      break;                                                                   \
    case llvm::DS_Warning:                                                     \
      DiagID = diag::warn_fe_##GroupName;                                      \
      break;                                                                   \
    case llvm::DS_Remark:                                                      \
      llvm_unreachable("'remark' severity not expected");                      \
      break;                                                                   \
    case llvm::DS_Note:                                                        \
      DiagID = diag::note_fe_##GroupName;                                      \
      break;                                                                   \
    }                                                                          \
  } while (false)

#define ComputeDiagRemarkID(Severity, GroupName, DiagID)                       \
  do {                                                                         \
    switch (Severity) {                                                        \
    case llvm::DS_Error:                                                       \
      DiagID = diag::err_fe_##GroupName;                                       \
      break;                                                                   \
    case llvm::DS_Warning:                                                     \
      DiagID = diag::warn_fe_##GroupName;                                      \
      break;                                                                   \
    case llvm::DS_Remark:                                                      \
      DiagID = diag::remark_fe_##GroupName;                                    \
      break;                                                                   \
    case llvm::DS_Note:                                                        \
      DiagID = diag::note_fe_##GroupName;                                      \
      break;                                                                   \
    }                                                                          \
  } while (false)

bool
BackendConsumer::InlineAsmDiagHandler(const llvm::DiagnosticInfoInlineAsm &D) {
  unsigned DiagID;
  ComputeDiagID(D.getSeverity(), inline_asm, DiagID);
  std::string Message = D.getMsgStr().str();

  // If this problem has clang-level source location information, report the
  // issue as being a problem in the source with a note showing the instantiated
  // code.
  SourceLocation LocCookie =
      SourceLocation::getFromRawEncoding(D.getLocCookie());
  if (LocCookie.isValid())
    Diags.Report(LocCookie, DiagID).AddString(Message);
  else {
    // Otherwise, report the backend diagnostic as occurring in the generated
    // .s file.
    // If Loc is invalid, we still need to report the diagnostic, it just gets
    // no location info.
    FullSourceLoc Loc;
    Diags.Report(Loc, DiagID).AddString(Message);
  }
  // We handled all the possible severities.
  return true;
}

bool
BackendConsumer::StackSizeDiagHandler(const llvm::DiagnosticInfoStackSize &D) {
  if (D.getSeverity() != llvm::DS_Warning)
    // For now, the only support we have for StackSize diagnostic is warning.
    // We do not know how to format other severities.
    return false;

  if (const Decl *ND = Gen->GetDeclForMangledName(D.getFunction().getName())) {
    // FIXME: Shouldn't need to truncate to uint32_t
    Diags.Report(ND->getASTContext().getFullLoc(ND->getLocation()),
                 diag::warn_fe_frame_larger_than)
      << static_cast<uint32_t>(D.getStackSize()) << Decl::castToDeclContext(ND);
    return true;
  }

  return false;
}

const FullSourceLoc BackendConsumer::getBestLocationFromDebugLoc(
    const llvm::DiagnosticInfoWithLocationBase &D, bool &BadDebugInfo,
    StringRef &Filename, unsigned &Line, unsigned &Column) const {
  SourceManager &SourceMgr = Context->getSourceManager();
  FileManager &FileMgr = SourceMgr.getFileManager();
  SourceLocation DILoc;

  if (D.isLocationAvailable()) {
    D.getLocation(Filename, Line, Column);
    if (Line > 0) {
      auto FE = FileMgr.getFile(Filename);
      if (!FE)
        FE = FileMgr.getFile(D.getAbsolutePath());
      if (FE) {
        // If -gcolumn-info was not used, Column will be 0. This upsets the
        // source manager, so pass 1 if Column is not set.
        DILoc = SourceMgr.translateFileLineCol(*FE, Line, Column ? Column : 1);
      }
    }
    BadDebugInfo = DILoc.isInvalid();
  }

  // If a location isn't available, try to approximate it using the associated
  // function definition. We use the definition's right brace to differentiate
  // from diagnostics that genuinely relate to the function itself.
  FullSourceLoc Loc(DILoc, SourceMgr);
  if (Loc.isInvalid())
    if (const Decl *FD = Gen->GetDeclForMangledName(D.getFunction().getName()))
      Loc = FD->getASTContext().getFullLoc(FD->getLocation());

  if (DILoc.isInvalid() && D.isLocationAvailable())
    // If we were not able to translate the file:line:col information
    // back to a SourceLocation, at least emit a note stating that
    // we could not translate this location. This can happen in the
    // case of #line directives.
    Diags.Report(Loc, diag::note_fe_backend_invalid_loc)
        << Filename << Line << Column;

  return Loc;
}

void BackendConsumer::UnsupportedDiagHandler(
    const llvm::DiagnosticInfoUnsupported &D) {
  // We only support warnings or errors.
  assert(D.getSeverity() == llvm::DS_Error ||
         D.getSeverity() == llvm::DS_Warning);

  StringRef Filename;
  unsigned Line, Column;
  bool BadDebugInfo = false;
  FullSourceLoc Loc;
  std::string Msg;
  raw_string_ostream MsgStream(Msg);

  // Context will be nullptr for IR input files, we will construct the diag
  // message from llvm::DiagnosticInfoUnsupported.
  if (Context != nullptr) {
    Loc = getBestLocationFromDebugLoc(D, BadDebugInfo, Filename, Line, Column);
    MsgStream << D.getMessage();
  } else {
    DiagnosticPrinterRawOStream DP(MsgStream);
    D.print(DP);
  }

  auto DiagType = D.getSeverity() == llvm::DS_Error
                      ? diag::err_fe_backend_unsupported
                      : diag::warn_fe_backend_unsupported;
  Diags.Report(Loc, DiagType) << MsgStream.str();

  if (BadDebugInfo)
    // If we were not able to translate the file:line:col information
    // back to a SourceLocation, at least emit a note stating that
    // we could not translate this location. This can happen in the
    // case of #line directives.
    Diags.Report(Loc, diag::note_fe_backend_invalid_loc)
        << Filename << Line << Column;
}

void BackendConsumer::EmitOptimizationMessage(
    const llvm::DiagnosticInfoOptimizationBase &D, unsigned DiagID) {
  // We only support warnings and remarks.
  assert(D.getSeverity() == llvm::DS_Remark ||
         D.getSeverity() == llvm::DS_Warning);

  StringRef Filename;
  unsigned Line, Column;
  bool BadDebugInfo = false;
  FullSourceLoc Loc;
  std::string Msg;
  raw_string_ostream MsgStream(Msg);

  // Context will be nullptr for IR input files, we will construct the remark
  // message from llvm::DiagnosticInfoOptimizationBase.
  if (Context != nullptr) {
    Loc = getBestLocationFromDebugLoc(D, BadDebugInfo, Filename, Line, Column);
    MsgStream << D.getMsg();
  } else {
    DiagnosticPrinterRawOStream DP(MsgStream);
    D.print(DP);
  }

  if (D.getHotness())
    MsgStream << " (hotness: " << *D.getHotness() << ")";

  Diags.Report(Loc, DiagID)
      << AddFlagValue(D.getPassName())
      << MsgStream.str();

  if (BadDebugInfo)
    // If we were not able to translate the file:line:col information
    // back to a SourceLocation, at least emit a note stating that
    // we could not translate this location. This can happen in the
    // case of #line directives.
    Diags.Report(Loc, diag::note_fe_backend_invalid_loc)
        << Filename << Line << Column;
}

void BackendConsumer::OptimizationRemarkHandler(
    const llvm::DiagnosticInfoOptimizationBase &D) {
  // Without hotness information, don't show noisy remarks.
  if (D.isVerbose() && !D.getHotness())
    return;

  if (D.isPassed()) {
    // Optimization remarks are active only if the -Rpass flag has a regular
    // expression that matches the name of the pass name in \p D.
    if (CodeGenOpts.OptimizationRemarkPattern &&
        CodeGenOpts.OptimizationRemarkPattern->match(D.getPassName()))
      EmitOptimizationMessage(D, diag::remark_fe_backend_optimization_remark);
  } else if (D.isMissed()) {
    // Missed optimization remarks are active only if the -Rpass-missed
    // flag has a regular expression that matches the name of the pass
    // name in \p D.
    if (CodeGenOpts.OptimizationRemarkMissedPattern &&
        CodeGenOpts.OptimizationRemarkMissedPattern->match(D.getPassName()))
      EmitOptimizationMessage(
          D, diag::remark_fe_backend_optimization_remark_missed);
  } else {
    assert(D.isAnalysis() && "Unknown remark type");

    bool ShouldAlwaysPrint = false;
    if (auto *ORA = dyn_cast<llvm::OptimizationRemarkAnalysis>(&D))
      ShouldAlwaysPrint = ORA->shouldAlwaysPrint();

    if (ShouldAlwaysPrint ||
        (CodeGenOpts.OptimizationRemarkAnalysisPattern &&
         CodeGenOpts.OptimizationRemarkAnalysisPattern->match(D.getPassName())))
      EmitOptimizationMessage(
          D, diag::remark_fe_backend_optimization_remark_analysis);
  }
}

void BackendConsumer::OptimizationRemarkHandler(
    const llvm::OptimizationRemarkAnalysisFPCommute &D) {
  // Optimization analysis remarks are active if the pass name is set to
  // llvm::DiagnosticInfo::AlwasyPrint or if the -Rpass-analysis flag has a
  // regular expression that matches the name of the pass name in \p D.

  if (D.shouldAlwaysPrint() ||
      (CodeGenOpts.OptimizationRemarkAnalysisPattern &&
       CodeGenOpts.OptimizationRemarkAnalysisPattern->match(D.getPassName())))
    EmitOptimizationMessage(
        D, diag::remark_fe_backend_optimization_remark_analysis_fpcommute);
}

void BackendConsumer::OptimizationRemarkHandler(
    const llvm::OptimizationRemarkAnalysisAliasing &D) {
  // Optimization analysis remarks are active if the pass name is set to
  // llvm::DiagnosticInfo::AlwasyPrint or if the -Rpass-analysis flag has a
  // regular expression that matches the name of the pass name in \p D.

  if (D.shouldAlwaysPrint() ||
      (CodeGenOpts.OptimizationRemarkAnalysisPattern &&
       CodeGenOpts.OptimizationRemarkAnalysisPattern->match(D.getPassName())))
    EmitOptimizationMessage(
        D, diag::remark_fe_backend_optimization_remark_analysis_aliasing);
}

void BackendConsumer::OptimizationFailureHandler(
    const llvm::DiagnosticInfoOptimizationFailure &D) {
  EmitOptimizationMessage(D, diag::warn_fe_backend_optimization_failure);
}

/// This function is invoked when the backend needs
/// to report something to the user.
void BackendConsumer::DiagnosticHandlerImpl(const DiagnosticInfo &DI) {
  unsigned DiagID = diag::err_fe_inline_asm;
  llvm::DiagnosticSeverity Severity = DI.getSeverity();
  // Get the diagnostic ID based.
  switch (DI.getKind()) {
  case llvm::DK_InlineAsm:
    if (InlineAsmDiagHandler(cast<DiagnosticInfoInlineAsm>(DI)))
      return;
    ComputeDiagID(Severity, inline_asm, DiagID);
    break;
  case llvm::DK_StackSize:
    if (StackSizeDiagHandler(cast<DiagnosticInfoStackSize>(DI)))
      return;
    ComputeDiagID(Severity, backend_frame_larger_than, DiagID);
    break;
  case DK_Linker:
    assert(CurLinkModule);
    // FIXME: stop eating the warnings and notes.
    if (Severity != DS_Error)
      return;
    DiagID = diag::err_fe_cannot_link_module;
    break;
  case llvm::DK_OptimizationRemark:
    // Optimization remarks are always handled completely by this
    // handler. There is no generic way of emitting them.
    OptimizationRemarkHandler(cast<OptimizationRemark>(DI));
    return;
  case llvm::DK_OptimizationRemarkMissed:
    // Optimization remarks are always handled completely by this
    // handler. There is no generic way of emitting them.
    OptimizationRemarkHandler(cast<OptimizationRemarkMissed>(DI));
    return;
  case llvm::DK_OptimizationRemarkAnalysis:
    // Optimization remarks are always handled completely by this
    // handler. There is no generic way of emitting them.
    OptimizationRemarkHandler(cast<OptimizationRemarkAnalysis>(DI));
    return;
  case llvm::DK_OptimizationRemarkAnalysisFPCommute:
    // Optimization remarks are always handled completely by this
    // handler. There is no generic way of emitting them.
    OptimizationRemarkHandler(cast<OptimizationRemarkAnalysisFPCommute>(DI));
    return;
  case llvm::DK_OptimizationRemarkAnalysisAliasing:
    // Optimization remarks are always handled completely by this
    // handler. There is no generic way of emitting them.
    OptimizationRemarkHandler(cast<OptimizationRemarkAnalysisAliasing>(DI));
    return;
  case llvm::DK_MachineOptimizationRemark:
    // Optimization remarks are always handled completely by this
    // handler. There is no generic way of emitting them.
    OptimizationRemarkHandler(cast<MachineOptimizationRemark>(DI));
    return;
  case llvm::DK_MachineOptimizationRemarkMissed:
    // Optimization remarks are always handled completely by this
    // handler. There is no generic way of emitting them.
    OptimizationRemarkHandler(cast<MachineOptimizationRemarkMissed>(DI));
    return;
  case llvm::DK_MachineOptimizationRemarkAnalysis:
    // Optimization remarks are always handled completely by this
    // handler. There is no generic way of emitting them.
    OptimizationRemarkHandler(cast<MachineOptimizationRemarkAnalysis>(DI));
    return;
  case llvm::DK_OptimizationFailure:
    // Optimization failures are always handled completely by this
    // handler.
    OptimizationFailureHandler(cast<DiagnosticInfoOptimizationFailure>(DI));
    return;
  case llvm::DK_Unsupported:
    UnsupportedDiagHandler(cast<DiagnosticInfoUnsupported>(DI));
    return;
  default:
    // Plugin IDs are not bound to any value as they are set dynamically.
    ComputeDiagRemarkID(Severity, backend_plugin, DiagID);
    break;
  }
  std::string MsgStorage;
  {
    raw_string_ostream Stream(MsgStorage);
    DiagnosticPrinterRawOStream DP(Stream);
    DI.print(DP);
  }

  if (DiagID == diag::err_fe_cannot_link_module) {
    Diags.Report(diag::err_fe_cannot_link_module)
        << CurLinkModule->getModuleIdentifier() << MsgStorage;
    return;
  }

  // Report the backend message using the usual diagnostic mechanism.
  FullSourceLoc Loc;
  Diags.Report(Loc, DiagID).AddString(MsgStorage);
}
#undef ComputeDiagID

CodeGenAction::CodeGenAction(LLVMContext *_VMContext)
    : VMContext(_VMContext ? _VMContext : new LLVMContext),
      OwnsVMContext(!_VMContext) {}

CodeGenAction::~CodeGenAction() {
  if (OwnsVMContext)
    delete VMContext;
}

std::unique_ptr<ASTConsumer>
CodeGenAction::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
  // BackendAction BA = static_cast<BackendAction>(Act);
  std::unique_ptr<raw_pwrite_stream> OS = CI.takeOutputStream();
  if (!OS)
    OS = CI.createDefaultOutputFile(false, InFile, "ssa.c");

  CI.getCodeGenOpts().OptimizationLevel = 1;
  CI.getCodeGenOpts().UnrollLoops = true;
  std::unique_ptr<BackendConsumer> Result(new BackendConsumer(
      CI.getDiagnostics(), CI.getHeaderSearchOpts(),
      CI.getPreprocessorOpts(), CI.getCodeGenOpts(), CI.getTargetOpts(),
      CI.getLangOpts(), std::string(InFile),
      std::move(OS), *VMContext, nullptr));
  BEConsumer = Result.get();

  return std::move(Result);
}

//