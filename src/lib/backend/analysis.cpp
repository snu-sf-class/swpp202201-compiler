#include "analysis.h"

#include <set>

namespace {
std::set<unsigned int> MoveInsts = {
    llvm::Instruction::BitCast, llvm::Instruction::PtrToInt,
    llvm::Instruction::IntToPtr, llvm::Instruction::ZExt,
    llvm::Instruction::Trunc};
std::set<unsigned int> NoRegInsts = {
    llvm::Instruction::Store, llvm::Instruction::Ret, llvm::Instruction::Switch,
    llvm::Instruction::Br};
} // namespace

namespace sc::backend::analysis {
llvm::Instruction *isMoveInst(llvm::Value *v) {
  llvm::Instruction *i = llvm::dyn_cast<llvm::Instruction>(v);
  return (i && MoveInsts.count(i->getOpcode())) ? i : nullptr;
}

bool isReg(llvm::Value *v) {
  while (llvm::Instruction *inst = isMoveInst(v))
    v = inst->getOperand(0);
  llvm::Instruction *I = llvm::dyn_cast<llvm::Instruction>(v);
  llvm::CallInst *CI = llvm::dyn_cast<llvm::CallInst>(v);
  return !(llvm::isa<llvm::Constant>(v) || llvm::isa<llvm::Argument>(v) ||
           (I && NoRegInsts.count(I->getOpcode())) ||
           (CI && (CI->getType()->isVoidTy() ||
                   CI->getCalledFunction()->getName().equals("$decr_sp"))));
}

uint64_t getSize(llvm::Type *T) {
  if (llvm::isa<llvm::PointerType>(T))
    return 8UL;
  if (llvm::isa<llvm::IntegerType>(T))
    return (T->getIntegerBitWidth() + 7UL) / 8UL;
  if (llvm::isa<llvm::ArrayType>(T))
    return analysis::getSize(T->getArrayElementType()) *
           T->getArrayNumElements();
  assert(false && "Unsupported access size type.");
}
} // namespace sc::backend::analysis
