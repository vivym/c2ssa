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
#include "llvm/Transforms/Coroutines.h"
#include "llvm/Transforms/Coroutines/CoroCleanup.h"
#include "llvm/Transforms/Coroutines/CoroEarly.h"
#include "llvm/Transforms/Coroutines/CoroElide.h"
#include "llvm/Transforms/Coroutines/CoroSplit.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/IPO/LowerTypeTests.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO/ThinLTOBitcodeWriter.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Instrumentation.h"
#include "llvm/Transforms/Instrumentation/AddressSanitizer.h"
#include "llvm/Transforms/Instrumentation/BoundsChecking.h"
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
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/LowerMatrixIntrinsics.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/CanonicalizeAliases.h"
#include "llvm/Transforms/Utils/EntryExitInstrumenter.h"
#include "llvm/Transforms/Utils/NameAnonGlobals.h"
#include "llvm/Transforms/Utils/SymbolRewriter.h"
#include "llvm/Transforms/Utils/UniqueInternalLinkageNames.h"
#include "PrintingPass.h"
#include <memory>
using namespace clang;
using namespace llvm;
using namespace c2ssa;

namespace {

// Default filename used for profile generation.
static constexpr StringLiteral DefaultProfileGenName = "default_%m.profraw";

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
    if (TM)
      return TM->getTargetIRAnalysis();

    return TargetIRAnalysis();
  }

  void CreatePasses(legacy::PassManager &MPM, legacy::FunctionPassManager &FPM);

  /// Generates the TargetMachine.
  /// Leaves TM unchanged if it is unable to create the target machine.
  /// Some of our clang tests specify triples which are not built
  /// into clang. This is okay because these tests check the generated
  /// IR, and they require DataLayout which depends on the triple.
  /// In this case, we allow this method to fail and not report an error.
  /// When MustCreateTM is used, we print an error if we are unable to load
  /// the requested target.
  void CreateTargetMachine(bool MustCreateTM);

  std::unique_ptr<llvm::ToolOutputFile> openOutputFile(StringRef Path) {
    std::error_code EC;
    auto F = std::make_unique<llvm::ToolOutputFile>(Path, EC,
                                                     llvm::sys::fs::OF_None);
    if (EC) {
      Diags.Report(diag::err_fe_unable_to_open_output) << Path << EC.message();
      F.reset();
    }
    return F;
  }

public:
  EmitAssemblyHelper(DiagnosticsEngine &_Diags,
                     const HeaderSearchOptions &HeaderSearchOpts,
                     const CodeGenOptions &CGOpts,
                     const clang::TargetOptions &TOpts,
                     const LangOptions &LOpts, Module *M)
      : Diags(_Diags), HSOpts(HeaderSearchOpts), CodeGenOpts(CGOpts),
        TargetOpts(TOpts), LangOpts(LOpts), TheModule(M),
        CodeGenerationTime("codegen", "Code Generation Time") {}

  ~EmitAssemblyHelper() {
    if (CodeGenOpts.DisableFree)
      BuryPointer(std::move(TM));
  }

  std::unique_ptr<TargetMachine> TM;

  void EmitAssemblyWithNewPassManager(std::unique_ptr<raw_pwrite_stream> OS);
};

// We need this wrapper to access LangOpts and CGOpts from extension functions
// that we add to the PassManagerBuilder.
class PassManagerBuilderWrapper : public PassManagerBuilder {
public:
  PassManagerBuilderWrapper(const Triple &TargetTriple,
                            const CodeGenOptions &CGOpts,
                            const LangOptions &LangOpts)
      : PassManagerBuilder(), TargetTriple(TargetTriple), CGOpts(CGOpts),
        LangOpts(LangOpts) {}
  const Triple &getTargetTriple() const { return TargetTriple; }
  const CodeGenOptions &getCGOpts() const { return CGOpts; }
  const LangOptions &getLangOpts() const { return LangOpts; }

private:
  const Triple &TargetTriple;
  const CodeGenOptions &CGOpts;
  const LangOptions &LangOpts;
};
}

static void addObjCARCAPElimPass(const PassManagerBuilder &Builder, PassManagerBase &PM) {
  if (Builder.OptLevel > 0)
    PM.add(createObjCARCAPElimPass());
}

static void addObjCARCExpandPass(const PassManagerBuilder &Builder, PassManagerBase &PM) {
  if (Builder.OptLevel > 0)
    PM.add(createObjCARCExpandPass());
}

static void addObjCARCOptPass(const PassManagerBuilder &Builder, PassManagerBase &PM) {
  if (Builder.OptLevel > 0)
    PM.add(createObjCARCOptPass());
}

static void addAddDiscriminatorsPass(const PassManagerBuilder &Builder,
                                     legacy::PassManagerBase &PM) {
  PM.add(createAddDiscriminatorsPass());
}

static void addBoundsCheckingPass(const PassManagerBuilder &Builder,
                                  legacy::PassManagerBase &PM) {
  PM.add(createBoundsCheckingLegacyPass());
}

static SanitizerCoverageOptions
getSancovOptsFromCGOpts(const CodeGenOptions &CGOpts) {
  SanitizerCoverageOptions Opts;
  Opts.CoverageType =
      static_cast<SanitizerCoverageOptions::Type>(CGOpts.SanitizeCoverageType);
  Opts.IndirectCalls = CGOpts.SanitizeCoverageIndirectCalls;
  Opts.TraceBB = CGOpts.SanitizeCoverageTraceBB;
  Opts.TraceCmp = CGOpts.SanitizeCoverageTraceCmp;
  Opts.TraceDiv = CGOpts.SanitizeCoverageTraceDiv;
  Opts.TraceGep = CGOpts.SanitizeCoverageTraceGep;
  Opts.Use8bitCounters = CGOpts.SanitizeCoverage8bitCounters;
  Opts.TracePC = CGOpts.SanitizeCoverageTracePC;
  Opts.TracePCGuard = CGOpts.SanitizeCoverageTracePCGuard;
  Opts.NoPrune = CGOpts.SanitizeCoverageNoPrune;
  Opts.Inline8bitCounters = CGOpts.SanitizeCoverageInline8bitCounters;
  Opts.InlineBoolFlag = CGOpts.SanitizeCoverageInlineBoolFlag;
  Opts.PCTable = CGOpts.SanitizeCoveragePCTable;
  Opts.StackDepth = CGOpts.SanitizeCoverageStackDepth;
  return Opts;
}

static void addSanitizerCoveragePass(const PassManagerBuilder &Builder,
                                     legacy::PassManagerBase &PM) {
  const PassManagerBuilderWrapper &BuilderWrapper =
      static_cast<const PassManagerBuilderWrapper &>(Builder);
  const CodeGenOptions &CGOpts = BuilderWrapper.getCGOpts();
  auto Opts = getSancovOptsFromCGOpts(CGOpts);
  PM.add(createModuleSanitizerCoverageLegacyPassPass(
      Opts, CGOpts.SanitizeCoverageAllowlistFiles,
      CGOpts.SanitizeCoverageBlocklistFiles));
}

// Check if ASan should use GC-friendly instrumentation for globals.
// First of all, there is no point if -fdata-sections is off (expect for MachO,
// where this is not a factor). Also, on ELF this feature requires an assembler
// extension that only works with -integrated-as at the moment.
static bool asanUseGlobalsGC(const Triple &T, const CodeGenOptions &CGOpts) {
  if (!CGOpts.SanitizeAddressGlobalsDeadStripping)
    return false;
  switch (T.getObjectFormat()) {
  case Triple::MachO:
  case Triple::COFF:
    return true;
  case Triple::ELF:
    return CGOpts.DataSections && !CGOpts.DisableIntegratedAS;
  case Triple::GOFF:
    llvm::report_fatal_error("ASan not implemented for GOFF");
  case Triple::XCOFF:
    llvm::report_fatal_error("ASan not implemented for XCOFF.");
  case Triple::Wasm:
  case Triple::UnknownObjectFormat:
    break;
  }
  return false;
}

static void addMemProfilerPasses(const PassManagerBuilder &Builder,
                                 legacy::PassManagerBase &PM) {
  PM.add(createMemProfilerFunctionPass());
  PM.add(createModuleMemProfilerLegacyPassPass());
}

static void addAddressSanitizerPasses(const PassManagerBuilder &Builder,
                                      legacy::PassManagerBase &PM) {
  const PassManagerBuilderWrapper &BuilderWrapper =
      static_cast<const PassManagerBuilderWrapper&>(Builder);
  const Triple &T = BuilderWrapper.getTargetTriple();
  const CodeGenOptions &CGOpts = BuilderWrapper.getCGOpts();
  bool Recover = CGOpts.SanitizeRecover.has(SanitizerKind::Address);
  bool UseAfterScope = CGOpts.SanitizeAddressUseAfterScope;
  bool UseOdrIndicator = CGOpts.SanitizeAddressUseOdrIndicator;
  bool UseGlobalsGC = asanUseGlobalsGC(T, CGOpts);
  PM.add(createAddressSanitizerFunctionPass(/*CompileKernel*/ false, Recover,
                                            UseAfterScope));
  PM.add(createModuleAddressSanitizerLegacyPassPass(
      /*CompileKernel*/ false, Recover, UseGlobalsGC, UseOdrIndicator));
}

static void addKernelAddressSanitizerPasses(const PassManagerBuilder &Builder,
                                            legacy::PassManagerBase &PM) {
  PM.add(createAddressSanitizerFunctionPass(
      /*CompileKernel*/ true, /*Recover*/ true, /*UseAfterScope*/ false));
  PM.add(createModuleAddressSanitizerLegacyPassPass(
      /*CompileKernel*/ true, /*Recover*/ true, /*UseGlobalsGC*/ true,
      /*UseOdrIndicator*/ false));
}

static void addHWAddressSanitizerPasses(const PassManagerBuilder &Builder,
                                            legacy::PassManagerBase &PM) {
  const PassManagerBuilderWrapper &BuilderWrapper =
      static_cast<const PassManagerBuilderWrapper &>(Builder);
  const CodeGenOptions &CGOpts = BuilderWrapper.getCGOpts();
  bool Recover = CGOpts.SanitizeRecover.has(SanitizerKind::HWAddress);
  PM.add(
      createHWAddressSanitizerLegacyPassPass(/*CompileKernel*/ false, Recover));
}

static void addKernelHWAddressSanitizerPasses(const PassManagerBuilder &Builder,
                                            legacy::PassManagerBase &PM) {
  PM.add(createHWAddressSanitizerLegacyPassPass(
      /*CompileKernel*/ true, /*Recover*/ true));
}

static void addGeneralOptsForMemorySanitizer(const PassManagerBuilder &Builder,
                                             legacy::PassManagerBase &PM,
                                             bool CompileKernel) {
  const PassManagerBuilderWrapper &BuilderWrapper =
      static_cast<const PassManagerBuilderWrapper&>(Builder);
  const CodeGenOptions &CGOpts = BuilderWrapper.getCGOpts();
  int TrackOrigins = CGOpts.SanitizeMemoryTrackOrigins;
  bool Recover = CGOpts.SanitizeRecover.has(SanitizerKind::Memory);
  PM.add(createMemorySanitizerLegacyPassPass(
      MemorySanitizerOptions{TrackOrigins, Recover, CompileKernel}));

  // MemorySanitizer inserts complex instrumentation that mostly follows
  // the logic of the original code, but operates on "shadow" values.
  // It can benefit from re-running some general purpose optimization passes.
  if (Builder.OptLevel > 0) {
    PM.add(createEarlyCSEPass());
    PM.add(createReassociatePass());
    PM.add(createLICMPass());
    PM.add(createGVNPass());
    PM.add(createInstructionCombiningPass());
    PM.add(createDeadStoreEliminationPass());
  }
}

static void addMemorySanitizerPass(const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
  addGeneralOptsForMemorySanitizer(Builder, PM, /*CompileKernel*/ false);
}

static void addKernelMemorySanitizerPass(const PassManagerBuilder &Builder,
                                         legacy::PassManagerBase &PM) {
  addGeneralOptsForMemorySanitizer(Builder, PM, /*CompileKernel*/ true);
}

static void addThreadSanitizerPass(const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
  PM.add(createThreadSanitizerLegacyPassPass());
}

static void addDataFlowSanitizerPass(const PassManagerBuilder &Builder,
                                     legacy::PassManagerBase &PM) {
  const PassManagerBuilderWrapper &BuilderWrapper =
      static_cast<const PassManagerBuilderWrapper&>(Builder);
  const LangOptions &LangOpts = BuilderWrapper.getLangOpts();
  PM.add(
      createDataFlowSanitizerLegacyPassPass(LangOpts.SanitizerBlacklistFiles));
}

static TargetLibraryInfoImpl *createTLII(llvm::Triple &TargetTriple,
                                         const CodeGenOptions &CodeGenOpts) {
  TargetLibraryInfoImpl *TLII = new TargetLibraryInfoImpl(TargetTriple);

  switch (CodeGenOpts.getVecLib()) {
  case CodeGenOptions::Accelerate:
    TLII->addVectorizableFunctionsFromVecLib(TargetLibraryInfoImpl::Accelerate);
    break;
  case CodeGenOptions::LIBMVEC:
    switch(TargetTriple.getArch()) {
      default:
        break;
      case llvm::Triple::x86_64:
        TLII->addVectorizableFunctionsFromVecLib
                (TargetLibraryInfoImpl::LIBMVEC_X86);
        break;
    }
    break;
  case CodeGenOptions::MASSV:
    TLII->addVectorizableFunctionsFromVecLib(TargetLibraryInfoImpl::MASSV);
    break;
  case CodeGenOptions::SVML:
    TLII->addVectorizableFunctionsFromVecLib(TargetLibraryInfoImpl::SVML);
    break;
  default:
    break;
  }
  return TLII;
}

static void addSymbolRewriterPass(const CodeGenOptions &Opts,
                                  legacy::PassManager *MPM) {
  llvm::SymbolRewriter::RewriteDescriptorList DL;

  llvm::SymbolRewriter::RewriteMapParser MapParser;
  for (const auto &MapFile : Opts.RewriteMapFiles)
    MapParser.parse(MapFile, &DL);

  MPM->add(createRewriteSymbolsPass(DL));
}

static CodeGenOpt::Level getCGOptLevel(const CodeGenOptions &CodeGenOpts) {
  switch (CodeGenOpts.OptimizationLevel) {
  default:
    llvm_unreachable("Invalid optimization level!");
  case 0:
    return CodeGenOpt::None;
  case 1:
    return CodeGenOpt::Less;
  case 2:
    return CodeGenOpt::Default; // O2/Os/Oz
  case 3:
    return CodeGenOpt::Aggressive;
  }
}

static Optional<llvm::CodeModel::Model>
getCodeModel(const CodeGenOptions &CodeGenOpts) {
  unsigned CodeModel = llvm::StringSwitch<unsigned>(CodeGenOpts.CodeModel)
                           .Case("tiny", llvm::CodeModel::Tiny)
                           .Case("small", llvm::CodeModel::Small)
                           .Case("kernel", llvm::CodeModel::Kernel)
                           .Case("medium", llvm::CodeModel::Medium)
                           .Case("large", llvm::CodeModel::Large)
                           .Case("default", ~1u)
                           .Default(~0u);
  assert(CodeModel != ~0u && "invalid code model!");
  if (CodeModel == ~1u)
    return None;
  return static_cast<llvm::CodeModel::Model>(CodeModel);
}

static bool initTargetOptions(DiagnosticsEngine &Diags,
                              llvm::TargetOptions &Options,
                              const CodeGenOptions &CodeGenOpts,
                              const clang::TargetOptions &TargetOpts,
                              const LangOptions &LangOpts,
                              const HeaderSearchOptions &HSOpts) {
  switch (LangOpts.getThreadModel()) {
  case LangOptions::ThreadModelKind::POSIX:
    Options.ThreadModel = llvm::ThreadModel::POSIX;
    break;
  case LangOptions::ThreadModelKind::Single:
    Options.ThreadModel = llvm::ThreadModel::Single;
    break;
  }

  // Set float ABI type.
  assert((CodeGenOpts.FloatABI == "soft" || CodeGenOpts.FloatABI == "softfp" ||
          CodeGenOpts.FloatABI == "hard" || CodeGenOpts.FloatABI.empty()) &&
         "Invalid Floating Point ABI!");
  Options.FloatABIType =
      llvm::StringSwitch<llvm::FloatABI::ABIType>(CodeGenOpts.FloatABI)
          .Case("soft", llvm::FloatABI::Soft)
          .Case("softfp", llvm::FloatABI::Soft)
          .Case("hard", llvm::FloatABI::Hard)
          .Default(llvm::FloatABI::Default);

  // Set FP fusion mode.
  switch (LangOpts.getDefaultFPContractMode()) {
  case LangOptions::FPM_Off:
    // Preserve any contraction performed by the front-end.  (Strict performs
    // splitting of the muladd intrinsic in the backend.)
    Options.AllowFPOpFusion = llvm::FPOpFusion::Standard;
    break;
  case LangOptions::FPM_On:
  case LangOptions::FPM_FastHonorPragmas:
    Options.AllowFPOpFusion = llvm::FPOpFusion::Standard;
    break;
  case LangOptions::FPM_Fast:
    Options.AllowFPOpFusion = llvm::FPOpFusion::Fast;
    break;
  }

  Options.UseInitArray = CodeGenOpts.UseInitArray;
  Options.DisableIntegratedAS = CodeGenOpts.DisableIntegratedAS;
  Options.CompressDebugSections = CodeGenOpts.getCompressDebugSections();
  Options.RelaxELFRelocations = CodeGenOpts.RelaxELFRelocations;

  // Set EABI version.
  Options.EABIVersion = TargetOpts.EABIVersion;

  if (LangOpts.hasSjLjExceptions())
    Options.ExceptionModel = llvm::ExceptionHandling::SjLj;
  if (LangOpts.hasSEHExceptions())
    Options.ExceptionModel = llvm::ExceptionHandling::WinEH;
  if (LangOpts.hasDWARFExceptions())
    Options.ExceptionModel = llvm::ExceptionHandling::DwarfCFI;
  if (LangOpts.hasWasmExceptions())
    Options.ExceptionModel = llvm::ExceptionHandling::Wasm;

  Options.NoInfsFPMath = LangOpts.NoHonorInfs;
  Options.NoNaNsFPMath = LangOpts.NoHonorNaNs;
  Options.NoZerosInBSS = CodeGenOpts.NoZeroInitializedInBSS;
  Options.UnsafeFPMath = LangOpts.UnsafeFPMath;
  Options.StackAlignmentOverride = CodeGenOpts.StackAlignment;

  Options.BBSections =
      llvm::StringSwitch<llvm::BasicBlockSection>(CodeGenOpts.BBSections)
          .Case("all", llvm::BasicBlockSection::All)
          .Case("labels", llvm::BasicBlockSection::Labels)
          .StartsWith("list=", llvm::BasicBlockSection::List)
          .Case("none", llvm::BasicBlockSection::None)
          .Default(llvm::BasicBlockSection::None);

  if (Options.BBSections == llvm::BasicBlockSection::List) {
    ErrorOr<std::unique_ptr<MemoryBuffer>> MBOrErr =
        MemoryBuffer::getFile(CodeGenOpts.BBSections.substr(5));
    if (!MBOrErr) {
      Diags.Report(diag::err_fe_unable_to_load_basic_block_sections_file)
          << MBOrErr.getError().message();
      return false;
    }
    Options.BBSectionsFuncListBuf = std::move(*MBOrErr);
  }

  Options.EnableMachineFunctionSplitter = CodeGenOpts.SplitMachineFunctions;
  Options.FunctionSections = CodeGenOpts.FunctionSections;
  Options.DataSections = CodeGenOpts.DataSections;
  Options.IgnoreXCOFFVisibility = CodeGenOpts.IgnoreXCOFFVisibility;
  Options.UniqueSectionNames = CodeGenOpts.UniqueSectionNames;
  Options.UniqueBasicBlockSectionNames =
      CodeGenOpts.UniqueBasicBlockSectionNames;
  Options.StackProtectorGuard =
      llvm::StringSwitch<llvm::StackProtectorGuards>(CodeGenOpts
          .StackProtectorGuard)
          .Case("tls", llvm::StackProtectorGuards::TLS)
          .Case("global", llvm::StackProtectorGuards::Global)
          .Default(llvm::StackProtectorGuards::None);
  Options.StackProtectorGuardOffset = CodeGenOpts.StackProtectorGuardOffset;
  Options.StackProtectorGuardReg = CodeGenOpts.StackProtectorGuardReg;
  Options.TLSSize = CodeGenOpts.TLSSize;
  Options.EmulatedTLS = CodeGenOpts.EmulatedTLS;
  Options.ExplicitEmulatedTLS = CodeGenOpts.ExplicitEmulatedTLS;
  Options.DebuggerTuning = CodeGenOpts.getDebuggerTuning();
  Options.EmitStackSizeSection = CodeGenOpts.StackSizeSection;
  Options.EmitAddrsig = CodeGenOpts.Addrsig;
  Options.ForceDwarfFrameSection = CodeGenOpts.ForceDwarfFrameSection;
  Options.EmitCallSiteInfo = CodeGenOpts.EmitCallSiteInfo;
  Options.EnableAIXExtendedAltivecABI = CodeGenOpts.EnableAIXExtendedAltivecABI;
  Options.PseudoProbeForProfiling = CodeGenOpts.PseudoProbeForProfiling;
  Options.ValueTrackingVariableLocations =
      CodeGenOpts.ValueTrackingVariableLocations;
  Options.XRayOmitFunctionIndex = CodeGenOpts.XRayOmitFunctionIndex;

  Options.MCOptions.SplitDwarfFile = CodeGenOpts.SplitDwarfFile;
  Options.MCOptions.MCRelaxAll = CodeGenOpts.RelaxAll;
  Options.MCOptions.MCSaveTempLabels = CodeGenOpts.SaveTempLabels;
  Options.MCOptions.MCUseDwarfDirectory = !CodeGenOpts.NoDwarfDirectoryAsm;
  Options.MCOptions.MCNoExecStack = CodeGenOpts.NoExecStack;
  Options.MCOptions.MCIncrementalLinkerCompatible =
      CodeGenOpts.IncrementalLinkerCompatible;
  Options.MCOptions.MCFatalWarnings = CodeGenOpts.FatalWarnings;
  Options.MCOptions.MCNoWarn = CodeGenOpts.NoWarn;
  Options.MCOptions.AsmVerbose = CodeGenOpts.AsmVerbose;
  Options.MCOptions.PreserveAsmComments = CodeGenOpts.PreserveAsmComments;
  Options.MCOptions.ABIName = TargetOpts.ABI;
  for (const auto &Entry : HSOpts.UserEntries)
    if (!Entry.IsFramework &&
        (Entry.Group == frontend::IncludeDirGroup::Quoted ||
         Entry.Group == frontend::IncludeDirGroup::Angled ||
         Entry.Group == frontend::IncludeDirGroup::System))
      Options.MCOptions.IASSearchPaths.push_back(
          Entry.IgnoreSysRoot ? Entry.Path : HSOpts.Sysroot + Entry.Path);
  Options.MCOptions.Argv0 = CodeGenOpts.Argv0;
  Options.MCOptions.CommandLineArgs = CodeGenOpts.CommandLineArgs;

  return true;
}

static Optional<GCOVOptions> getGCOVOptions(const CodeGenOptions &CodeGenOpts,
                                            const LangOptions &LangOpts) {
  if (!CodeGenOpts.EmitGcovArcs && !CodeGenOpts.EmitGcovNotes)
    return None;
  // Not using 'GCOVOptions::getDefault' allows us to avoid exiting if
  // LLVM's -default-gcov-version flag is set to something invalid.
  GCOVOptions Options;
  Options.EmitNotes = CodeGenOpts.EmitGcovNotes;
  Options.EmitData = CodeGenOpts.EmitGcovArcs;
  llvm::copy(CodeGenOpts.CoverageVersion, std::begin(Options.Version));
  Options.NoRedZone = CodeGenOpts.DisableRedZone;
  Options.Filter = CodeGenOpts.ProfileFilterFiles;
  Options.Exclude = CodeGenOpts.ProfileExcludeFiles;
  Options.Atomic = CodeGenOpts.AtomicProfileUpdate;
  return Options;
}

static Optional<InstrProfOptions>
getInstrProfOptions(const CodeGenOptions &CodeGenOpts,
                    const LangOptions &LangOpts) {
  if (!CodeGenOpts.hasProfileClangInstr())
    return None;
  InstrProfOptions Options;
  Options.NoRedZone = CodeGenOpts.DisableRedZone;
  Options.InstrProfileOutput = CodeGenOpts.InstrProfileOutput;
  Options.Atomic = CodeGenOpts.AtomicProfileUpdate;
  return Options;
}

void EmitAssemblyHelper::CreatePasses(legacy::PassManager &MPM,
                                      legacy::FunctionPassManager &FPM) {
  // Handle disabling of all LLVM passes, where we want to preserve the
  // internal module before any optimization.
  if (CodeGenOpts.DisableLLVMPasses)
    return;

  // Figure out TargetLibraryInfo.  This needs to be added to MPM and FPM
  // manually (and not via PMBuilder), since some passes (eg. InstrProfiling)
  // are inserted before PMBuilder ones - they'd get the default-constructed
  // TLI with an unknown target otherwise.
  Triple TargetTriple(TheModule->getTargetTriple());
  std::unique_ptr<TargetLibraryInfoImpl> TLII(
      createTLII(TargetTriple, CodeGenOpts));

  // If we reached here with a non-empty index file name, then the index file
  // was empty and we are not performing ThinLTO backend compilation (used in
  // testing in a distributed build environment). Drop any the type test
  // assume sequences inserted for whole program vtables so that codegen doesn't
  // complain.
  if (!CodeGenOpts.ThinLTOIndexFile.empty())
    MPM.add(createLowerTypeTestsPass(/*ExportSummary=*/nullptr,
                                     /*ImportSummary=*/nullptr,
                                     /*DropTypeTests=*/true));

  PassManagerBuilderWrapper PMBuilder(TargetTriple, CodeGenOpts, LangOpts);

  // At O0 and O1 we only run the always inliner which is more efficient. At
  // higher optimization levels we run the normal inliner.
  if (CodeGenOpts.OptimizationLevel <= 1) {
    bool InsertLifetimeIntrinsics = ((CodeGenOpts.OptimizationLevel != 0 &&
                                      !CodeGenOpts.DisableLifetimeMarkers) ||
                                     LangOpts.Coroutines);
    PMBuilder.Inliner = createAlwaysInlinerLegacyPass(InsertLifetimeIntrinsics);
  } else {
    // We do not want to inline hot callsites for SamplePGO module-summary build
    // because profile annotation will happen again in ThinLTO backend, and we
    // want the IR of the hot path to match the profile.
    PMBuilder.Inliner = createFunctionInliningPass(
        CodeGenOpts.OptimizationLevel, CodeGenOpts.OptimizeSize,
        (!CodeGenOpts.SampleProfileFile.empty() &&
         CodeGenOpts.PrepareForThinLTO));
  }

  PMBuilder.OptLevel = CodeGenOpts.OptimizationLevel;
  PMBuilder.SizeLevel = CodeGenOpts.OptimizeSize;
  PMBuilder.SLPVectorize = CodeGenOpts.VectorizeSLP;
  PMBuilder.LoopVectorize = CodeGenOpts.VectorizeLoop;
  // Only enable CGProfilePass when using integrated assembler, since
  // non-integrated assemblers don't recognize .cgprofile section.
  PMBuilder.CallGraphProfile = !CodeGenOpts.DisableIntegratedAS;

  PMBuilder.DisableUnrollLoops = !CodeGenOpts.UnrollLoops;
  // Loop interleaving in the loop vectorizer has historically been set to be
  // enabled when loop unrolling is enabled.
  PMBuilder.LoopsInterleaved = CodeGenOpts.UnrollLoops;
  PMBuilder.MergeFunctions = CodeGenOpts.MergeFunctions;
  PMBuilder.PrepareForThinLTO = CodeGenOpts.PrepareForThinLTO;
  PMBuilder.PrepareForLTO = CodeGenOpts.PrepareForLTO;
  PMBuilder.RerollLoops = CodeGenOpts.RerollLoops;

  MPM.add(new TargetLibraryInfoWrapperPass(*TLII));

  if (TM)
    TM->adjustPassManager(PMBuilder);

  if (CodeGenOpts.DebugInfoForProfiling ||
      !CodeGenOpts.SampleProfileFile.empty())
    PMBuilder.addExtension(PassManagerBuilder::EP_EarlyAsPossible,
                           addAddDiscriminatorsPass);

  // In ObjC ARC mode, add the main ARC optimization passes.
  if (LangOpts.ObjCAutoRefCount) {
    PMBuilder.addExtension(PassManagerBuilder::EP_EarlyAsPossible,
                           addObjCARCExpandPass);
    PMBuilder.addExtension(PassManagerBuilder::EP_ModuleOptimizerEarly,
                           addObjCARCAPElimPass);
    PMBuilder.addExtension(PassManagerBuilder::EP_ScalarOptimizerLate,
                           addObjCARCOptPass);
  }

  if (LangOpts.Coroutines)
    addCoroutinePassesToExtensionPoints(PMBuilder);

  if (!CodeGenOpts.MemoryProfileOutput.empty()) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addMemProfilerPasses);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addMemProfilerPasses);
  }

  if (LangOpts.Sanitize.has(SanitizerKind::LocalBounds)) {
    PMBuilder.addExtension(PassManagerBuilder::EP_ScalarOptimizerLate,
                           addBoundsCheckingPass);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addBoundsCheckingPass);
  }

  if (CodeGenOpts.SanitizeCoverageType ||
      CodeGenOpts.SanitizeCoverageIndirectCalls ||
      CodeGenOpts.SanitizeCoverageTraceCmp) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addSanitizerCoveragePass);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addSanitizerCoveragePass);
  }

  if (LangOpts.Sanitize.has(SanitizerKind::Address)) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addAddressSanitizerPasses);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addAddressSanitizerPasses);
  }

  if (LangOpts.Sanitize.has(SanitizerKind::KernelAddress)) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addKernelAddressSanitizerPasses);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addKernelAddressSanitizerPasses);
  }

  if (LangOpts.Sanitize.has(SanitizerKind::HWAddress)) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addHWAddressSanitizerPasses);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addHWAddressSanitizerPasses);
  }

  if (LangOpts.Sanitize.has(SanitizerKind::KernelHWAddress)) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addKernelHWAddressSanitizerPasses);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addKernelHWAddressSanitizerPasses);
  }

  if (LangOpts.Sanitize.has(SanitizerKind::Memory)) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addMemorySanitizerPass);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addMemorySanitizerPass);
  }

  if (LangOpts.Sanitize.has(SanitizerKind::KernelMemory)) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addKernelMemorySanitizerPass);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addKernelMemorySanitizerPass);
  }

  if (LangOpts.Sanitize.has(SanitizerKind::Thread)) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addThreadSanitizerPass);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addThreadSanitizerPass);
  }

  if (LangOpts.Sanitize.has(SanitizerKind::DataFlow)) {
    PMBuilder.addExtension(PassManagerBuilder::EP_OptimizerLast,
                           addDataFlowSanitizerPass);
    PMBuilder.addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                           addDataFlowSanitizerPass);
  }

  // Set up the per-function pass manager.
  FPM.add(new TargetLibraryInfoWrapperPass(*TLII));
  if (CodeGenOpts.VerifyModule)
    FPM.add(createVerifierPass());

  // Set up the per-module pass manager.
  if (!CodeGenOpts.RewriteMapFiles.empty())
    addSymbolRewriterPass(CodeGenOpts, &MPM);

  // Add UniqueInternalLinkageNames Pass which renames internal linkage symbols
  // with unique names.
  if (CodeGenOpts.UniqueInternalLinkageNames) {
    MPM.add(createUniqueInternalLinkageNamesPass());
  }

  if (Optional<GCOVOptions> Options = getGCOVOptions(CodeGenOpts, LangOpts)) {
    MPM.add(createGCOVProfilerPass(*Options));
    if (CodeGenOpts.getDebugInfo() == codegenoptions::NoDebugInfo)
      MPM.add(createStripSymbolsPass(true));
  }

  if (Optional<InstrProfOptions> Options =
          getInstrProfOptions(CodeGenOpts, LangOpts))
    MPM.add(createInstrProfilingLegacyPass(*Options, false));

  bool hasIRInstr = false;
  if (CodeGenOpts.hasProfileIRInstr()) {
    PMBuilder.EnablePGOInstrGen = true;
    hasIRInstr = true;
  }
  if (CodeGenOpts.hasProfileCSIRInstr()) {
    assert(!CodeGenOpts.hasProfileCSIRUse() &&
           "Cannot have both CSProfileUse pass and CSProfileGen pass at the "
           "same time");
    assert(!hasIRInstr &&
           "Cannot have both ProfileGen pass and CSProfileGen pass at the "
           "same time");
    PMBuilder.EnablePGOCSInstrGen = true;
    hasIRInstr = true;
  }
  if (hasIRInstr) {
    if (!CodeGenOpts.InstrProfileOutput.empty())
      PMBuilder.PGOInstrGen = CodeGenOpts.InstrProfileOutput;
    else
      PMBuilder.PGOInstrGen = std::string(DefaultProfileGenName);
  }
  if (CodeGenOpts.hasProfileIRUse()) {
    PMBuilder.PGOInstrUse = CodeGenOpts.ProfileInstrumentUsePath;
    PMBuilder.EnablePGOCSInstrUse = CodeGenOpts.hasProfileCSIRUse();
  }

  if (!CodeGenOpts.SampleProfileFile.empty())
    PMBuilder.PGOSampleUse = CodeGenOpts.SampleProfileFile;

  PMBuilder.populateFunctionPassManager(FPM);
  PMBuilder.populateModulePassManager(MPM);
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
  BackendArgs.push_back(nullptr);
  llvm::cl::ParseCommandLineOptions(BackendArgs.size() - 1,
                                    BackendArgs.data());
}

void EmitAssemblyHelper::CreateTargetMachine(bool MustCreateTM) {
  // Create the TargetMachine for generating code.
  std::string Error;
  std::string Triple = TheModule->getTargetTriple();
  const llvm::Target *TheTarget = TargetRegistry::lookupTarget(Triple, Error);
  if (!TheTarget) {
    if (MustCreateTM)
      Diags.Report(diag::err_fe_unable_to_create_target) << Error;
    return;
  }

  Optional<llvm::CodeModel::Model> CM = getCodeModel(CodeGenOpts);
  std::string FeaturesStr =
      llvm::join(TargetOpts.Features.begin(), TargetOpts.Features.end(), ",");
  llvm::Reloc::Model RM = CodeGenOpts.RelocationModel;
  CodeGenOpt::Level OptLevel = getCGOptLevel(CodeGenOpts);

  llvm::TargetOptions Options;
  if (!initTargetOptions(Diags, Options, CodeGenOpts, TargetOpts, LangOpts,
                         HSOpts))
    return;
  TM.reset(TheTarget->createTargetMachine(Triple, TargetOpts.CPU, FeaturesStr,
                                          Options, RM, CM, OptLevel));
}

static PassBuilder::OptimizationLevel mapToLevel(const CodeGenOptions &Opts) {
  switch (Opts.OptimizationLevel) {
  default:
    llvm_unreachable("Invalid optimization level!");

  case 0:
    return PassBuilder::OptimizationLevel::O0;

  case 1:
    return PassBuilder::OptimizationLevel::O1;

  case 2:
    switch (Opts.OptimizeSize) {
    default:
      llvm_unreachable("Invalid optimization level for size!");

    case 0:
      return PassBuilder::OptimizationLevel::O2;

    case 1:
      return PassBuilder::OptimizationLevel::Os;

    case 2:
      return PassBuilder::OptimizationLevel::Oz;
    }

  case 3:
    return PassBuilder::OptimizationLevel::O3;
  }
}

/// A clean version of `EmitAssembly` that uses the new pass manager.
///
/// Not all features are currently supported in this system, but where
/// necessary it falls back to the legacy pass manager to at least provide
/// basic functionality.
///
/// This API is planned to have its functionality finished and then to replace
/// `EmitAssembly` at some point in the future when the default switches.
void EmitAssemblyHelper::EmitAssemblyWithNewPassManager(std::unique_ptr<raw_pwrite_stream> OS) {
  TimeRegion Region(CodeGenOpts.TimePasses ? &CodeGenerationTime : nullptr);
  setCommandLineOpts(CodeGenOpts);

  CreateTargetMachine(false);

  if (TM)
    TheModule->setDataLayout(TM->createDataLayout());

  PipelineTuningOptions PTO;
  PTO.LoopUnrolling = CodeGenOpts.UnrollLoops;
  // For historical reasons, loop interleaving is set to mirror setting for loop
  // unrolling.
  PTO.LoopInterleaving = CodeGenOpts.UnrollLoops;
  PTO.LoopVectorization = CodeGenOpts.VectorizeLoop;
  PTO.SLPVectorization = CodeGenOpts.VectorizeSLP;
  PTO.MergeFunctions = CodeGenOpts.MergeFunctions;
  // Only enable CGProfilePass when using integrated assembler, since
  // non-integrated assemblers don't recognize .cgprofile section.
  PTO.CallGraphProfile = !CodeGenOpts.DisableIntegratedAS;
  PTO.Coroutines = LangOpts.Coroutines;

  PassInstrumentationCallbacks PIC;
  StandardInstrumentations SI(CodeGenOpts.DebugPassManager);
  SI.registerCallbacks(PIC);
  Optional<PGOOptions> PGOOpt;
  PassBuilder PB(CodeGenOpts.DebugPassManager, TM.get(), PTO, PGOOpt, &PIC);

  LoopAnalysisManager LAM(CodeGenOpts.DebugPassManager);
  FunctionAnalysisManager FAM(CodeGenOpts.DebugPassManager);
  CGSCCAnalysisManager CGAM(CodeGenOpts.DebugPassManager);
  ModuleAnalysisManager MAM(CodeGenOpts.DebugPassManager);

  // Register the AA manager first so that our version is the one used.
  FAM.registerPass([&] { return PB.buildDefaultAAPipeline(); });

  // Register the target library analysis directly and give it a customized
  // preset TLI.
  Triple TargetTriple(TheModule->getTargetTriple());
  std::unique_ptr<TargetLibraryInfoImpl> TLII(
      createTLII(TargetTriple, CodeGenOpts));
  FAM.registerPass([&] { return TargetLibraryAnalysis(*TLII); });

  // Register all the basic analyses with the managers.
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  ModulePassManager MPM(CodeGenOpts.DebugPassManager);

  if (!CodeGenOpts.DisableLLVMPasses) {
    // Map our optimization levels into one of the distinct levels used to
    // configure the pipeline.
    PassBuilder::OptimizationLevel Level = mapToLevel(CodeGenOpts);

    bool IsThinLTO = CodeGenOpts.PrepareForThinLTO;
    bool IsLTO = CodeGenOpts.PrepareForLTO;

    // If we reached here with a non-empty index file name, then the index
    // file was empty and we are not performing ThinLTO backend compilation
    // (used in testing in a distributed build environment). Drop any the type
    // test assume sequences inserted for whole program vtables so that
    // codegen doesn't complain.
    if (!CodeGenOpts.ThinLTOIndexFile.empty())
      PB.registerPipelineStartEPCallback(
          [](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
            MPM.addPass(LowerTypeTestsPass(/*ExportSummary=*/nullptr,
                                           /*ImportSummary=*/nullptr,
                                           /*DropTypeTests=*/true));
          });

    if (Level != PassBuilder::OptimizationLevel::O0) {
      PB.registerPipelineStartEPCallback(
          [](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
            MPM.addPass(createModuleToFunctionPassAdaptor(
                EntryExitInstrumenterPass(/*PostInlining=*/false)));
          });
    }

    // Register callbacks to schedule sanitizer passes at the appropriate part
    // of the pipeline.
    if (LangOpts.Sanitize.has(SanitizerKind::LocalBounds))
      PB.registerScalarOptimizerLateEPCallback(
          [](FunctionPassManager &FPM, PassBuilder::OptimizationLevel Level) {
            FPM.addPass(BoundsCheckingPass());
          });

    if (CodeGenOpts.SanitizeCoverageType ||
        CodeGenOpts.SanitizeCoverageIndirectCalls ||
        CodeGenOpts.SanitizeCoverageTraceCmp) {
      PB.registerOptimizerLastEPCallback(
          [this](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
            auto SancovOpts = getSancovOptsFromCGOpts(CodeGenOpts);
            MPM.addPass(ModuleSanitizerCoveragePass(
                SancovOpts, CodeGenOpts.SanitizeCoverageAllowlistFiles,
                CodeGenOpts.SanitizeCoverageBlocklistFiles));
          });
    }

    if (LangOpts.Sanitize.has(SanitizerKind::Memory)) {
      int TrackOrigins = CodeGenOpts.SanitizeMemoryTrackOrigins;
      bool Recover = CodeGenOpts.SanitizeRecover.has(SanitizerKind::Memory);
      PB.registerOptimizerLastEPCallback(
          [TrackOrigins, Recover](ModulePassManager &MPM,
                                  PassBuilder::OptimizationLevel Level) {
            MPM.addPass(MemorySanitizerPass({TrackOrigins, Recover, false}));
            MPM.addPass(createModuleToFunctionPassAdaptor(
                MemorySanitizerPass({TrackOrigins, Recover, false})));
          });
    }
    if (LangOpts.Sanitize.has(SanitizerKind::Thread)) {
      PB.registerOptimizerLastEPCallback(
          [](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
            MPM.addPass(ThreadSanitizerPass());
            MPM.addPass(
                createModuleToFunctionPassAdaptor(ThreadSanitizerPass()));
          });
    }

    auto ASanPass = [&](SanitizerMask Mask, bool CompileKernel) {
      if (LangOpts.Sanitize.has(Mask)) {
        bool Recover = CodeGenOpts.SanitizeRecover.has(Mask);
        bool UseAfterScope = CodeGenOpts.SanitizeAddressUseAfterScope;
        bool ModuleUseAfterScope = asanUseGlobalsGC(TargetTriple, CodeGenOpts);
        bool UseOdrIndicator = CodeGenOpts.SanitizeAddressUseOdrIndicator;
        PB.registerOptimizerLastEPCallback(
            [CompileKernel, Recover, UseAfterScope, ModuleUseAfterScope,
             UseOdrIndicator](ModulePassManager &MPM,
                              PassBuilder::OptimizationLevel Level) {
              MPM.addPass(
                  RequireAnalysisPass<ASanGlobalsMetadataAnalysis, Module>());
              MPM.addPass(ModuleAddressSanitizerPass(CompileKernel, Recover,
                                                     ModuleUseAfterScope,
                                                     UseOdrIndicator));
              MPM.addPass(createModuleToFunctionPassAdaptor(
                  AddressSanitizerPass(CompileKernel, Recover, UseAfterScope)));
            });
      }
    };
    ASanPass(SanitizerKind::Address, false);
    ASanPass(SanitizerKind::KernelAddress, true);

    auto HWASanPass = [&](SanitizerMask Mask, bool CompileKernel) {
      if (LangOpts.Sanitize.has(Mask)) {
        bool Recover = CodeGenOpts.SanitizeRecover.has(Mask);
        PB.registerOptimizerLastEPCallback(
            [CompileKernel, Recover](ModulePassManager &MPM,
                                     PassBuilder::OptimizationLevel Level) {
              MPM.addPass(HWAddressSanitizerPass(CompileKernel, Recover));
            });
      }
    };
    HWASanPass(SanitizerKind::HWAddress, false);
    HWASanPass(SanitizerKind::KernelHWAddress, true);

    if (LangOpts.Sanitize.has(SanitizerKind::DataFlow)) {
      PB.registerOptimizerLastEPCallback(
          [this](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
            MPM.addPass(
                DataFlowSanitizerPass(LangOpts.SanitizerBlacklistFiles));
          });
    }

    if (Optional<GCOVOptions> Options = getGCOVOptions(CodeGenOpts, LangOpts))
      PB.registerPipelineStartEPCallback(
          [Options](ModulePassManager &MPM,
                    PassBuilder::OptimizationLevel Level) {
            MPM.addPass(GCOVProfilerPass(*Options));
          });
    if (Optional<InstrProfOptions> Options =
            getInstrProfOptions(CodeGenOpts, LangOpts))
      PB.registerPipelineStartEPCallback(
          [Options](ModulePassManager &MPM,
                    PassBuilder::OptimizationLevel Level) {
            MPM.addPass(InstrProfiling(*Options, false));
          });

    if (CodeGenOpts.OptimizationLevel == 0) {
      MPM = PB.buildO0DefaultPipeline(Level, IsLTO || IsThinLTO);
    } else if (IsThinLTO) {
      MPM = PB.buildThinLTOPreLinkDefaultPipeline(Level);
    } else if (IsLTO) {
      MPM = PB.buildLTOPreLinkDefaultPipeline(Level);
    } else {
      MPM = PB.buildPerModuleDefaultPipeline(Level);
    }

    // Add UniqueInternalLinkageNames Pass which renames internal linkage
    // symbols with unique names.
    if (CodeGenOpts.UniqueInternalLinkageNames)
      MPM.addPass(UniqueInternalLinkageNamesPass());

    if (!CodeGenOpts.MemoryProfileOutput.empty()) {
      MPM.addPass(createModuleToFunctionPassAdaptor(MemProfilerPass()));
      MPM.addPass(ModuleMemProfilerPass());
    }
  }

  // Append any output we need to the pass manager.
  // MPM.addPass(c2ssa::PrintModulePass(*OS, "", CodeGenOpts.EmitLLVMUseLists));
  MPM.addPass(createModuleToFunctionPassAdaptor(c2ssa::PrintModulePass(*OS, "", CodeGenOpts.EmitLLVMUseLists)));

  // Before executing passes, print the final values of the LLVM options.
  cl::PrintOptionValues();

  // Now that we have all of the passes ready, run them.
  {
    PrettyStackTraceString CrashInfo("Optimizer");
    MPM.run(*TheModule, MAM);
  }
}

void c2ssa::EmitBackendOutput(DiagnosticsEngine &Diags,
                              const HeaderSearchOptions &HeaderOpts,
                              const CodeGenOptions &CGOpts,
                              const clang::TargetOptions &TOpts,
                              const LangOptions &LOpts,
                              const llvm::DataLayout &TDesc, Module *M,
                              std::unique_ptr<raw_pwrite_stream> OS) {

  llvm::TimeTraceScope TimeScope("Backend");

  EmitAssemblyHelper AsmHelper(Diags, HeaderOpts, CGOpts, TOpts, LOpts, M);
  AsmHelper.EmitAssemblyWithNewPassManager(std::move(OS));

  // Verify clang's TargetInfo DataLayout against the LLVM TargetMachine's
  // DataLayout.
  if (AsmHelper.TM) {
    std::string DLDesc = M->getDataLayout().getStringRepresentation();
    if (DLDesc != TDesc.getStringRepresentation()) {
      unsigned DiagID = Diags.getCustomDiagID(
          DiagnosticsEngine::Error, "backend data layout '%0' does not match "
                                    "expected target description '%1'");
      Diags.Report(DiagID) << DLDesc << TDesc.getStringRepresentation();
    }
  }
}
