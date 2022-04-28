#include "malloc_make.h"

#include "llvm/IR/Instructions.h"

namespace sc::backend::malloc_mk {
llvm::PreservedAnalyses MallocMakePass::run(llvm::Module &M,
                                            llvm::ModuleAnalysisManager &MAM) {
  if (*malloc_size) {
    llvm::Function *F = M.getFunction("main");
    assert(F && "No main function.");
    llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
    llvm::FunctionCallee FC = M.getOrInsertFunction(
        "malloc", llvm::Type::getInt8PtrTy(M.getContext()), Int64Ty);
    llvm::ConstantInt *Const =
        llvm::ConstantInt::get(Int64Ty, *malloc_size, true);
    llvm::Instruction *I = F->getEntryBlock().getFirstNonPHI();
    assert(I && "There is no non-phi instruction inside entry block.");
    llvm::CallInst *CI = llvm::CallInst::Create(FC, {Const}, "", I);
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::malloc_mk
