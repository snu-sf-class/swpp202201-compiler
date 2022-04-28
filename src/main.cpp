#include "args.h"
#include "lib.h"
#include "result.h"

#include <iostream>
#include <string>

int main(int argc, char *argv[]) {
  auto argparse_result = scargs::parse(argc, argv);
  if (argparse_result.isErr()) {
    const auto err =
        decltype(argparse_result)::inspect(std::move(argparse_result));
    std::cerr << err.what() << "\n";
    return 1;
  }

  auto args = decltype(argparse_result)::unwrap(std::move(argparse_result));
  auto compile_result = sc::compile(args.input_filename, args.output_filename,
                                    args.verbose_printing);
  auto ret = decltype(compile_result)::unwrapOrElse(
      std::move(compile_result), [](auto &&e) {
        std::cerr << e.what() << "\n";
        return 1;
      });
  return ret;
}
