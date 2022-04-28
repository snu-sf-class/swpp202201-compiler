#include "backend.h"

#include "backend/assembly.h"
#include "backend/emitter.h"
#include "backend/const_expr_eliminate.h"
#include "backend/gep_eliminate.h"
#include "backend/gv_eliminate.h"
#include "backend/alloca_eliminate.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "backend/malloc_make.h"
#include "backend/phi_preprocess.h"
#include "backend/register_allocate.h"
#include "print_ir.h"

using namespace std::string_literals;

namespace sc::backend {
BackendInternalError::BackendInternalError(const std::exception &__e) noexcept {
  message = "exception thrown from backend\n"s + __e.what();
}

Result<std::string, BackendInternalError>
emitAssembly(std::unique_ptr<llvm::Module> &&__M,
             llvm::ModuleAnalysisManager &__MAM) noexcept {
  using RetType = Result<std::string, BackendInternalError>;

  symbol::SymbolMap SM;
  llvm::ModulePassManager MPM;
  llvm::FunctionPassManager FPM;
  uint64_t malloc_size;
  try {
    MPM.addPass(ce_elim::ConstExprEliminatePass());
    MPM.addPass(gep_elim::GEPEliminatePass());
    MPM.addPass(gv_elim::GVEliminatePass(malloc_size));
    MPM.addPass(alloca_elim::AllocaEliminatePass());
    FPM.addPass(llvm::InstCombinePass());
    MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
    MPM.addPass(malloc_mk::MallocMakePass(malloc_size));
    MPM.addPass(ce_elim::ConstExprEliminatePass());
    MPM.addPass(phi_prep::PHIPreprocessPass());
    MPM.addPass(reg_alloc::RegisterAllocatePass(SM));
    MPM.run(*__M, __MAM);
  } catch (const std::exception &e) {
    return RetType::Err(BackendInternalError(e));
  }

  sc::print_ir::printIRIfVerbose(*__M, "After backend passes"s);

  std::string assembly;
  try {
    auto AE = emitter::AssemblyEmitter(SM);
    AE.visit(*__M);
    assembly = AE.getAssembly();
  } catch (const std::exception &e) {
    return RetType::Err(BackendInternalError(e));
  }

  return RetType::Ok(assembly);
}
} // namespace sc::backend
