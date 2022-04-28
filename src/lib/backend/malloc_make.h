#ifndef SC_BACKEND_MALLOC_MAKE_H
#define SC_BACKEND_MALLOC_MAKE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::malloc_mk {

class MallocMakePass : public llvm::PassInfoMixin<MallocMakePass> {

public:
  MallocMakePass(uint64_t &malloc_size_) : malloc_size(&malloc_size_) {};
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);

private:
  uint64_t *malloc_size;
};
} // namespace sc::backend::malloc_mk
#endif // SC_BACKEND_MALLOC_MAKE_H
