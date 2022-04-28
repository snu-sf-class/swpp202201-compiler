#include "alloca_eliminate.h"

#include "analysis.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"

namespace sc::backend::alloca_elim {
llvm::PreservedAnalyses
AllocaEliminatePass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
  llvm::FunctionCallee decr_sp =
      M.getOrInsertFunction("$decr_sp", Int64Ty, Int64Ty);
  std::vector<llvm::Instruction *> trashBin;

  for (llvm::Function &F : M) {
    uint64_t acc = 0UL;
    for (llvm::BasicBlock &BB : F)
      for (llvm::Instruction &I : BB) {
        if (llvm::AllocaInst *AI = llvm::dyn_cast<llvm::AllocaInst>(&I)) {
          assert(AI->isStaticAlloca() && "alloca is not static.");
          acc += (analysis::getSize(AI->getAllocatedType()) + 7UL) / 8UL * 8UL;
        }
      }
    if (acc) {
      llvm::Instruction *FI = F.getEntryBlock().getFirstNonPHI();
      llvm::ConstantInt *Const = llvm::ConstantInt::get(Int64Ty, acc, true);
      llvm::Value *Args[] = {Const};
      llvm::CallInst *CI = llvm::CallInst::Create(
          decr_sp, llvm::ArrayRef<llvm::Value *>(Args), "", FI);
      acc = 0UL;
      trashBin.clear();
      for (llvm::BasicBlock &BB : F)
        for (llvm::Instruction &I : BB)
          if (llvm::AllocaInst *AI = llvm::dyn_cast<llvm::AllocaInst>(&I)) {
            llvm::ConstantInt *offset =
                llvm::ConstantInt::get(Int64Ty, acc, true);
            llvm::BinaryOperator *Sub =
                llvm::BinaryOperator::CreateAdd(CI, offset, "", AI);
            llvm::CastInst *Cast = llvm::CastInst::CreateBitOrPointerCast(
                Sub, AI->getType(), "", AI);
            AI->replaceAllUsesWith(Cast);
            trashBin.emplace_back(AI);
            acc +=
                (analysis::getSize(AI->getAllocatedType()) + 7UL) / 8UL * 8UL;
          }
      for (llvm::Instruction *AI : trashBin)
        AI->eraseFromParent();
    }
  }
  bool updated = true;
  while (updated) {
    updated = false;
    trashBin.clear();
    for (llvm::Function &F : M)
      for (llvm::BasicBlock &BB : F)
        for (llvm::Instruction &I : BB)
          if (llvm::IntToPtrInst *ITPI =
                  llvm::dyn_cast<llvm::IntToPtrInst>(&I)) {
            if (ITPI->hasNUses(0)) {
              updated = true;
              trashBin.emplace_back(ITPI);
            } else
              for (llvm::User *U : ITPI->users())
                if (llvm::PtrToIntInst *PTII =
                        llvm::dyn_cast<llvm::PtrToIntInst>(U)) {
                  updated = true;
                  llvm::Value *oper = ITPI->getOperand(0);
                  U->replaceAllUsesWith(oper);
                  trashBin.emplace_back(PTII);
                }
          }
    for (llvm::Instruction *I : trashBin)
      I->eraseFromParent();
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::alloca_elim
