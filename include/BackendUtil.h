#ifndef CODEGEN_BACKENDUTIL_H
#define CODEGEN_BACKENDUTIL_H

#include "clang/Basic/LLVM.h"
#include "llvm/IR/ModuleSummaryIndex.h"
#include <memory>

namespace llvm {
  class BitcodeModule;
  template <typename T> class Expected;
  class Module;
  class MemoryBufferRef;
}

namespace clang {
  class DiagnosticsEngine;
  class HeaderSearchOptions;
  class CodeGenOptions;
  class TargetOptions;
  class LangOptions;
}

namespace c2ssa {
  void EmitBackendOutput(clang::DiagnosticsEngine &Diags, const clang::HeaderSearchOptions &,
                         const clang::CodeGenOptions &CGOpts,
                         const clang::TargetOptions &TOpts, const clang::LangOptions &LOpts,
                         const llvm::DataLayout &TDesc, llvm::Module *M,
                         std::unique_ptr<clang::raw_pwrite_stream> OS);
}

#endif
