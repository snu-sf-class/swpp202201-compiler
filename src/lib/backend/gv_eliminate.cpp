#include "gv_eliminate.h"

#include "analysis.h"
#include "llvm/IR/Instructions.h"

using namespace std::string_literals;

namespace {
constexpr uint64_t START_ADDRESS = 204800UL;

class NoMainError : public Error<NoMainError> {
public:
  const char *what() const noexcept {
    return "GV elimination error: no main function to bind the GV";
  }
};

class InvalidEntryBlockError : public Error<InvalidEntryBlockError> {
public:
  const char *what() const noexcept {
    return "GV elimination error: unable to bind the malloc into the entry "
           "block";
  }
};

template <typename E> class ErrorWithGV : public Error<ErrorWithGV<E>> {
private:
  std::string message;

public:
  ErrorWithGV(E &&__err, const llvm::GlobalVariable &__gv) noexcept {
    std::string gv;
    llvm::raw_string_ostream rso(gv);
    rso << __gv;

    message = std::string(__err.what()).append("\n["s).append(gv).append("]"s);
  }

  const char *what() const noexcept { return message.c_str(); }
};

template <typename T, typename E>
T unwrapOrThrowWithGV(Result<T, E> &&__res, const llvm::GlobalVariable &__gv) {
  using ResType = Result<T, E>;

  if (__res.isErr()) {
    auto err = ResType::inspect(std::move(__res));
    throw ErrorWithGV(std::move(err), __gv);
  }

  return ResType::unwrap(std::move(__res));
}
} // namespace

namespace sc::backend::gv_elim {
llvm::PreservedAnalyses GVEliminatePass::run(llvm::Module &M,
                                             llvm::ModuleAnalysisManager &MAM) {
  llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
  uint64_t acc = 0UL;
  std::vector<llvm::PtrToIntInst *> trashBin;
  for (llvm::GlobalVariable &gv : M.globals()) {
    uint64_t addr = START_ADDRESS + acc;
    gv.setName("$" + std::to_string(addr));
    const auto gv_size =
        unwrapOrThrowWithGV(analysis::tryCalculateSize(gv.getValueType()), gv);
    const auto alloc_size = (gv_size + 7UL) & (UINT64_MAX - 7UL);
    acc += alloc_size;
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
    if (!F) {
      throw NoMainError();
    }

    llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
    llvm::FunctionCallee FC = M.getOrInsertFunction(
        "malloc", llvm::Type::getInt8PtrTy(M.getContext()), Int64Ty);
    llvm::ConstantInt *Const = llvm::ConstantInt::get(Int64Ty, acc, true);
    llvm::Instruction *I = F->getEntryBlock().getFirstNonPHI();
    if (!I) {
      throw InvalidEntryBlockError();
    }

    llvm::CallInst *CI = llvm::CallInst::Create(FC, {Const}, "", I);
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::gv_elim
