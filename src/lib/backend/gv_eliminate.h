#ifndef SC_BACKEND_GV_ELIMINATE_H
#define SC_BACKEND_GV_ELIMINATE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::gv_elim {
const uint64_t START_ADDRESS = 204800UL;

class GVEliminatePass : public llvm::PassInfoMixin<GVEliminatePass> {

public:
  GVEliminatePass(uint64_t &malloc_size_) : malloc_size(&malloc_size_) {};
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);

private:
  uint64_t *malloc_size;
};
} // namespace sc::backend::gv_elim
#endif // SC_BACKEND_GV_ELIMINATE_H
