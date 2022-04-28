#include "lib.h"

#include "fs.h"
#include "lib/backend.h"
#include "lib/opt.h"
#include "lib/parser.h"
#include "lib/print_ir.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Passes/PassBuilder.h"

#include <memory>

namespace {
class InputFileError : public Error<InputFileError> {
private:
  std::string message;

public:
  InputFileError(sc::parser::ParserError &&__err) noexcept {
    using namespace std::string_literals;
    message = "invalid input file\n"s.append(__err.what());
  }

  InputFileError(fs::FilesystemError &&__err) noexcept {
    using namespace std::string_literals;
    message = "invalid input file\n"s.append(__err.what());
  }

  const char *what() const noexcept { return message.c_str(); }
};

class OutputFileError : public Error<OutputFileError> {
private:
  std::string message;

public:
  OutputFileError(const std::string_view __message) {
    using namespace std::string_literals;
    message = "invalid output file\n"s;
    message.append(__message);
  }
  const char *what() const noexcept { return message.c_str(); }
};

Result<std::string, InputFileError>
readFile(const std::string_view __filename) {
  auto read_result = fs::readFile(__filename);
  return decltype(read_result)::mapErr<InputFileError>(
      std::move(read_result),
      [](auto &&err) { return InputFileError(std::move(err)); });
}

Result<size_t, OutputFileError> writeFile(const std::string_view __filename,
                                          const std::string_view __content) {
  auto write_result = fs::writeFile(__filename, __content);
  return decltype(write_result)::mapErr<OutputFileError>(
      std::move(write_result),
      [](auto &&err) { return OutputFileError(err.what()); });
}
} // namespace

namespace sc {
template <typename E>
SWPPCompilerError::SWPPCompilerError(Error<E> &&__err) noexcept {
  using namespace std::string_literals;
  message = "swpp-compiler crashed: "s.append(__err.what());
}

Result<size_t, SWPPCompilerError>
compile(const std::string_view __input_filename,
        const std::string_view __output_filename,
        const bool __verbose_printing = false) noexcept {
  auto read_result =
      Result<std::string, InputFileError>::mapErr<SWPPCompilerError>(
          readFile(__input_filename),
          [](auto &&err) { return SWPPCompilerError(std::move(err)); });

  llvm::LLVMContext context;

  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  llvm::PassBuilder PB;
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  if (__verbose_printing) {
    print_ir::setVerbose();
  }

  auto parse_result =
      decltype(read_result)::andThen<std::unique_ptr<llvm::Module>>(
          std::move(read_result),
          [&context, __input_filename](std::string &&code) {
            auto res = parser::parseIR(code, __input_filename, context);
            return decltype(res)::mapErr<SWPPCompilerError>(
                std::move(res), [](auto &&err) {
                  return SWPPCompilerError(InputFileError(std::move(err)));
                });
          });

  auto optimize_result =
      decltype(parse_result)::andThen<std::unique_ptr<llvm::Module>>(
          std::move(parse_result), [&MAM](std::unique_ptr<llvm::Module> &&M) {
            auto res = sc::opt::optimizeIR(std::move(M), MAM);
            return decltype(res)::mapErr<SWPPCompilerError>(
                std::move(res),
                [](auto &&err) { return SWPPCompilerError(std::move(err)); });
          });

  auto emit_result = decltype(optimize_result)::andThen<std::string>(
      std::move(optimize_result), [&MAM](std::unique_ptr<llvm::Module> &&M) {
        auto res = sc::backend::emitAssembly(std::move(M), MAM);
        return decltype(res)::mapErr<SWPPCompilerError>(
            std::move(res),
            [](auto &&err) { return SWPPCompilerError(std::move(err)); });
      });

  auto write_result = decltype(emit_result)::andThen<size_t>(
      std::move(emit_result), [__output_filename](std::string &&assembly) {
        auto res = writeFile(__output_filename, assembly);
        return decltype(res)::mapErr<SWPPCompilerError>(
            std::move(res),
            [](auto &&err) { return SWPPCompilerError(std::move(err)); });
      });

  auto compile_result = decltype(write_result)::map<size_t>(
      std::move(write_result), [](auto &&written_bytes) { return 0; });

  return compile_result;
}
} // namespace sc
