set(LLVM_INCLUDE_DIRS
  ${LLVM_ROOT_DIR}/llvm/include
  ${LLVM_ROOT_DIR}/clang/include
  ${LLVM_ROOT_DIR}/build/include
  ${LLVM_ROOT_DIR}/build/tools/clang/include
)

set(LLVM_LIBRARY_DIRS
  ${LLVM_ROOT_DIR}/build/lib
)

find_package(ZLIB)

set(SYSTEM_LIBS
  ZLIB::ZLIB      # Required by libLLVMSupport
  curses          # Required by libLLVMSupport
)

set(LLVM_LIBS
  clangAnalysis
  clangAPINotes
  clangApplyReplacements
  clangARCMigrate
  clangAST
  clangASTMatchers
  clangBasic
  clangChangeNamespace
  clangCodeGen
  clangCrossTU
  clangDaemon
  clangDaemonTweaks
  clangDependencyScanning
  clangDependencyScanning
  clangDirectoryWatcher
  clangDoc
  clangdRemoteIndex
  clangDriver
  clangdSupport
  clangdXpcJsonConversions
  clangdXpcTransport
  clangDynamicASTMatchers
  clangEdit
  clangFormat
  clangFrontend
  clangFrontendTool
  clangHandleCXX
  clangHandleLLVM
  clangIncludeFixer
  clangIncludeFixerPlugin
  clangIndex
  clangIndexSerialization
  clangLex
  clangMove
  clangParse
  clangQuery
  clangReorderFields
  clangRewrite
  clangRewriteFrontend
  clangSema
  clangSerialization
  clangStaticAnalyzerCheckers
  clangStaticAnalyzerCore
  clangStaticAnalyzerFrontend
  clangTesting
  clangTidy
  clangTidyAbseilModule
  clangTidyAlteraModule
  clangTidyAndroidModule
  clangTidyBoostModule
  clangTidyBugproneModule
  clangTidyCERTModule
  clangTidyConcurrencyModule
  clangTidyCppCoreGuidelinesModule
  clangTidyDarwinModule
  clangTidyFuchsiaModule
  clangTidyGoogleModule
  clangTidyHICPPModule
  clangTidyLinuxKernelModule
  clangTidyLLVMLibcModule
  clangTidyLLVMModule
  clangTidyMain
  clangTidyMiscModule
  clangTidyModernizeModule
  clangTidyMPIModule
  clangTidyObjCModule
  clangTidyOpenMPModule
  clangTidyPerformanceModule
  clangTidyPlugin
  clangTidyPortabilityModule
  clangTidyReadabilityModule
  clangTidyUtils
  clangTidyZirconModule
  clangTooling
  clangToolingASTDiff
  clangToolingCore
  clangToolingInclusions
  clangToolingRefactoring
  clangToolingSyntax
  clangTransformer
  DynamicLibraryLib
  findAllSymbols
  LLVMAArch64AsmParser
  LLVMAArch64CodeGen
  LLVMAArch64Desc
  LLVMAArch64Disassembler
  LLVMAArch64Info
  LLVMAArch64Utils
  LLVMAggressiveInstCombine
  LLVMAMDGPUAsmParser
  LLVMAMDGPUCodeGen
  LLVMAMDGPUDesc
  LLVMAMDGPUDisassembler
  LLVMAMDGPUInfo
  LLVMAMDGPUUtils
  LLVMAnalysis
  LLVMARMAsmParser
  LLVMARMCodeGen
  LLVMARMDesc
  LLVMARMDisassembler
  LLVMARMInfo
  LLVMARMUtils
  LLVMAsmParser
  LLVMAsmPrinter
  LLVMAVRAsmParser
  LLVMAVRCodeGen
  LLVMAVRDesc
  LLVMAVRDisassembler
  LLVMAVRInfo
  LLVMBinaryFormat
  LLVMBitReader
  LLVMBitstreamReader
  LLVMBitWriter
  LLVMBPFAsmParser
  LLVMBPFCodeGen
  LLVMBPFDesc
  LLVMBPFDisassembler
  LLVMBPFInfo
  LLVMCFGuard
  LLVMCFIVerify
  LLVMCodeGen
  LLVMCore
  LLVMCoroutines
  LLVMCoverage
  LLVMDebugInfoCodeView
  LLVMDebugInfoDWARF
  LLVMDebugInfoGSYM
  LLVMDebugInfoMSF
  LLVMDebugInfoPDB
  LLVMDemangle
  LLVMDlltoolDriver
  LLVMDWARFLinker
  LLVMExecutionEngine
  LLVMExegesis
  LLVMExegesisAArch64
  LLVMExegesisMips
  LLVMExegesisPowerPC
  LLVMExegesisX86
  LLVMExtensions
  LLVMFileCheck
  LLVMFrontendOpenACC
  LLVMFrontendOpenMP
  LLVMFuzzMutate
  LLVMGlobalISel
  LLVMHelloNew
  LLVMHexagonAsmParser
  LLVMHexagonCodeGen
  LLVMHexagonDesc
  LLVMHexagonDisassembler
  LLVMHexagonInfo
  LLVMInstCombine
  LLVMInstrumentation
  LLVMInterfaceStub
  LLVMInterpreter
  LLVMipo
  LLVMIRReader
  LLVMJITLink
  LLVMLanaiAsmParser
  LLVMLanaiCodeGen
  LLVMLanaiDesc
  LLVMLanaiDisassembler
  LLVMLanaiInfo
  LLVMLibDriver
  LLVMLineEditor
  LLVMLinker
  LLVMLTO
  LLVMMC
  LLVMMCA
  LLVMMCDisassembler
  LLVMMCJIT
  LLVMMCParser
  LLVMMipsAsmParser
  LLVMMipsCodeGen
  LLVMMipsDesc
  LLVMMipsDisassembler
  LLVMMipsInfo
  LLVMMIRParser
  LLVMMSP430AsmParser
  LLVMMSP430CodeGen
  LLVMMSP430Desc
  LLVMMSP430Disassembler
  LLVMMSP430Info
  LLVMNVPTXCodeGen
  LLVMNVPTXDesc
  LLVMNVPTXInfo
  LLVMObjCARCOpts
  LLVMObject
  LLVMObjectYAML
  LLVMOption
  LLVMOrcJIT
  LLVMOrcShared
  LLVMOrcTargetProcess
  LLVMPasses
  LLVMPowerPCAsmParser
  LLVMPowerPCCodeGen
  LLVMPowerPCDesc
  LLVMPowerPCDisassembler
  LLVMPowerPCInfo
  LLVMProfileData
  LLVMRemarks
  LLVMRISCVAsmParser
  LLVMRISCVCodeGen
  LLVMRISCVDesc
  LLVMRISCVDisassembler
  LLVMRISCVInfo
  LLVMRISCVUtils
  LLVMRuntimeDyld
  LLVMScalarOpts
  LLVMSelectionDAG
  LLVMSparcAsmParser
  LLVMSparcCodeGen
  LLVMSparcDesc
  LLVMSparcDisassembler
  LLVMSparcInfo
  LLVMSupport
  LLVMSymbolize
  LLVMSystemZAsmParser
  LLVMSystemZCodeGen
  LLVMSystemZDesc
  LLVMSystemZDisassembler
  LLVMSystemZInfo
  LLVMTableGen
  LLVMTableGenGlobalISel
  LLVMTarget
  LLVMTestingSupport
  LLVMTextAPI
  LLVMTransformUtils
  LLVMVectorize
  LLVMWebAssemblyAsmParser
  LLVMWebAssemblyCodeGen
  LLVMWebAssemblyDesc
  LLVMWebAssemblyDisassembler
  LLVMWebAssemblyInfo
  LLVMWindowsManifest
  LLVMX86AsmParser
  LLVMX86CodeGen
  LLVMX86Desc
  LLVMX86Disassembler
  LLVMX86Info
  LLVMXCoreCodeGen
  LLVMXCoreDesc
  LLVMXCoreDisassembler
  LLVMXCoreInfo
  LLVMXRay
  LLVMCore
)
