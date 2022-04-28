#ifndef SC_BACKEND_ANALYSIS_H
#define SC_BACKEND_ANALYSIS_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

namespace sc::backend::analysis {
llvm::Instruction *isMoveInst(llvm::Value *v);
bool isReg(llvm::Value *v);
uint64_t getSize(llvm::Type *T);
} // namespace sc::backend::analysis
#endif // SC_BACKEND_ANALYSIS_H
