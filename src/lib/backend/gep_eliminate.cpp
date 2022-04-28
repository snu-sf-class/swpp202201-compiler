#include "gep_eliminate.h"

#include "analysis.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"

namespace sc::backend::gep_elim {
llvm::PreservedAnalyses
GEPEliminatePass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
  std::set<llvm::GetElementPtrInst *> trashBin;
  for (llvm::Function &F : M) {
    trashBin.clear();
    for (llvm::BasicBlock &BB : F)
      for (llvm::Instruction &I : BB)
        if (llvm::GetElementPtrInst *GEPI =
                llvm::dyn_cast<llvm::GetElementPtrInst>(&I)) {

          llvm::Value *ptrOp = GEPI->getPointerOperand();
          llvm::Type *curr = ptrOp->getType();
          curr = curr->getPointerElementType();

          llvm::Instruction *pti =
              llvm::CastInst::CreateBitOrPointerCast(ptrOp, Int64Ty, "", GEPI);

          std::vector<llvm::Instruction *> v;
          v.push_back(pti);
          for (auto opIt = GEPI->idx_begin(); opIt != GEPI->idx_end(); ++opIt) {
            llvm::Value *op = *opIt;
            uint64_t size = analysis::getSize(curr);
            llvm::Instruction *mul = llvm::BinaryOperator::CreateMul(
                op, llvm::ConstantInt::get(Int64Ty, size, true), "", GEPI);
            llvm::Instruction *add =
                llvm::BinaryOperator::CreateAdd(v.back(), mul, "", GEPI);
            v.push_back(add);
            if (curr->isArrayTy())
              curr = curr->getArrayElementType();
          }

          llvm::Instruction *itp = llvm::CastInst::CreateBitOrPointerCast(
              v.back(), I.getType(), "", GEPI);
          GEPI->replaceAllUsesWith(itp);
          trashBin.insert(GEPI);
        }
    for (llvm::GetElementPtrInst *I : trashBin)
      I->eraseFromParent();
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::gep_elim
