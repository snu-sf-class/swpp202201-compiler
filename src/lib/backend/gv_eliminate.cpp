#include "gv_eliminate.h"

#include "analysis.h"
#include "llvm/IR/Instructions.h"

namespace sc::backend::gv_elim {
llvm::PreservedAnalyses GVEliminatePass::run(llvm::Module &M,
                                             llvm::ModuleAnalysisManager &MAM) {
  llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
  uint64_t acc = 0UL;
  std::vector<llvm::PtrToIntInst *> trashBin;
  for (llvm::GlobalVariable &gv : M.globals()) {
    uint64_t addr = START_ADDRESS + acc;
    gv.setName("$" + std::to_string(addr));
    acc += (analysis::getSize(gv.getValueType()) + 7UL) / 8UL * 8UL;
    for (llvm::User *user : gv.users())
      if (llvm::PtrToIntInst *PTII = llvm::dyn_cast<llvm::PtrToIntInst>(user)) {
        PTII->replaceAllUsesWith(llvm::ConstantInt::get(Int64Ty, addr, true));
        trashBin.push_back(PTII);
      }
  }
  for (llvm::PtrToIntInst *PTII : trashBin)
    PTII->eraseFromParent();
  if (acc) {
    llvm::Function *F = M.getFunction("main");
    assert(F && "No main function.");
    llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
    llvm::FunctionCallee FC = M.getOrInsertFunction(
        "malloc", llvm::Type::getInt8PtrTy(M.getContext()), Int64Ty);
    llvm::ConstantInt *Const = llvm::ConstantInt::get(Int64Ty, acc, true);
    llvm::Instruction *I = F->getEntryBlock().getFirstNonPHI();
    assert(I && "There is no non-phi instruction inside entry block.");
    llvm::CallInst *CI = llvm::CallInst::Create(FC, {Const}, "", I);
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::gv_elim
