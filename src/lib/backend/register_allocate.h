#ifndef SC_BACKEND_REGISTER_ALLOCATE_H
#define SC_BACKEND_REGISTER_ALLOCATE_H

#include "./symbol.h"
#include "llvm/IR/PassManager.h"

namespace sc::backend::reg_alloc {
class RegisterAllocatePass : public llvm::PassInfoMixin<RegisterAllocatePass> {
public:
  RegisterAllocatePass(symbol::SymbolMap &SM_) : SM(&SM_){};
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);

private:
  symbol::SymbolMap *SM;
};
} // namespace sc::backend::reg_alloc
#endif // SC_BACKEND_REGISTER_ALLOCATE_H
