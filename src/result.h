#ifndef SC_RESULT_H
#define SC_RESULT_H

/**
 * @file result.h
 * @author SWPP TAs (swpp@sf.snu.ac.kr)
 * @brief Header-only module for monadic results
 * @version 0.1
 * @date 2022-04-28
 * @copyright Copyright (c) 2022-2022 SWPP TAs
 */

#include "static_error.h"

#include <functional>
#include <variant>

namespace result {
/**
 * @brief Exception thrown by trying to unwrap the Err Result
 */
class BadUnwrapError : public Error<BadUnwrapError> {
public:
  /**
   * @brief Read the exception
   * @return Exception message in C-String format
   */
  const char *what() const noexcept {
    return "Result error: tried to unwrap Err";
  }
};

/**
 * @brief Exception thrown by trying to inspect the Ok Result
 */
class BadInspectError : public Error<BadInspectError> {
public:
  /**
   * @brief Read the exception
   * @return Exception message in C-String format
   */
  const char *what() const noexcept {
    return "Result error: tried to inspect Ok";
  }
};

/**
 * @brief Monadic result type for error handling without runtime overhead
 *
 * This represents the output of 'potentially failing' operations.
 *
 * Result can have one of the two states: Ok and Err.
 * * Ok contains the output from successful operation. The output can be later
 * extracted using the `Result::unwrap()`.
 * * Err contains the error from unsuccessful operation. The error can be later
 * extracted using the `Result::inspect()`.
 *
 * Monadic methods can be used to modify the output or propagte the error.
 * You can use these methods even if you don't understand the mathematical
 * definition of monad. Actually, in our context, it is just a fancy way of
 * saying "conditional apply".
 *
 * However, due to limitations in the c++ grammar, monads are implemented as
 * static member functions that take the rvalue reference of Result.
 * This approach unfortunately has many drawbacks.
 * * You have to re-write the exact type of your operand Result to call the
 * monad. Using `decltype` should be preferred for most of the cases.
 * * You have to use [std::move](https://en.cppreference.com/w/cpp/utility/move)
 * very frequently to use the Result as a monad operand. This means you cannot
 * use `const` Results, because `const` objects cannot be "moved".
 * * You have to explicitly specify the template parameter of mapping monads,
 * as compilers cannot deduce the return type of the monad from the function.
 * * As a result, using the Result is not exactly beautiful...
 * ```
 * auto result = operationThatYieldsSomeResult();
 * auto mapped_result = decltype(result)::map<U>(std::move(result),
 *                          [](auto &&data){ return data.convertToU(data); });
 * const U expected = decltype(mapped_result)::unwrap(std::move(mapped_result));
 * ```
 *
 * @tparam T type of return value upon success
 * @tparam E type of error upon failure
 */
template <typename T, typename E> class Result {
  using DataTy = std::variant<T, E>;

private:
  enum class Kind { OK, ERR };
  Kind kind;
  DataTy data;

  /**
   * @brief Construct a new Result object
   */
  constexpr Result(const Kind kind, DataTy &&data) noexcept
      : kind(kind), data(std::move(data)) {}

public:
  /**
   * @brief Move-construct a Result object
   * @param other Result to move from
   */
  constexpr Result(Result &&other) noexcept = default;

  /**
   * @brief Move-assign a Result object
   * @param other Result to move from
   */
  constexpr Result &operator=(Result &&other) noexcept = default;

  /**
   * @brief Check if the Result can be unwrapped
   * @return true if the Result is Ok
   * @return false if the Result is Err
   */
  constexpr bool isOk() const noexcept { return (kind == Kind::OK); }

  /**
   * @brief Check if the Result can be inspected
   * @return true if the Result is Err
   * @return false if the Result is Ok
   */
  constexpr bool isErr() const noexcept { return (kind == Kind::ERR); }

  /**
   * @brief Create a new Ok Result
   * @param data output to move into the Result
   * @return constexpr Result of Ok state
   */
  constexpr static Result Ok(T &&data) noexcept {
    return Result(Kind::OK, std::move(data));
  }

  /**
   * @brief Create a new Ok Result by copying
   * @param data output to copy into the Result
   * @return constexpr Result of Ok state
   */
  constexpr static Result Ok(const T &data) noexcept {
    return Result(Kind::OK, data);
  }

  /**
   * @brief Create a new Err Result
   * @param data output to move into the Result
   * @return constexpr Result of Ok state
   */
  constexpr static Result Err(E &&data) noexcept {
    return Result(Kind::ERR, std::move(data));
  }

  /**
   * @brief Create a new Err Result by copying
   * @param data output to copy into the Result
   * @return constexpr Result of Ok state
   */
  constexpr static Result Err(const E &data) noexcept {
    return Result(Kind::ERR, data);
  }

  /**
   * @brief Apply the given function to the Ok value, while leaving the Err
   * Result untouched
   * 
   * This monad computes a `Result<U, E>` from `Result<T, E>` using `U fn(&&T)`
   * * If the Result was Ok, apply `fn` to the contained value, and create 
   * a new Ok Result out of it
   * * If the Result was Err, type-cast a new Err Result without touching the
   * contained error
   * 
   * @tparam U return type of `fn`
   * @param src Result to compute new Result from
   * @param fn Function used to map the value
   * @return constexpr Result<U, E> See the description
   */
  template <typename U>
  constexpr static Result<U, E> map(Result &&src,
                                    std::function<U(T &&)> &&fn) noexcept {
    if (src.isOk()) {
      auto data = std::get<T>(std::move(src.data));
      return Result<U, E>::Ok(fn(std::move(data)));
    } else {
      auto data = std::get<E>(std::move(src.data));
      return Result<U, E>::Err(std::move(data));
    }
  }

  /**
   * @brief Apply the given function to the Err value, while leaving the Ok
   * Result untouched
   * 
   * This monad computes a `Result<T, F>` from `Result<T, E>` using `F fn(&&E)`
   * * If the Result was Err, apply `fn` to the contained error, and create 
   * a new Err Result out of it
   * * If the Result was Ok, type-cast a new Ok Result without touching the
   * contained value
   * 
   * @tparam F return type of `fn`
   * @param src Result to compute new Result from
   * @param fn Function used to map the error
   * @return constexpr Result<T, F> See the description
   */
  template <typename F>
  constexpr static Result<T, F> mapErr(Result &&src,
                                       std::function<F(E &&)> &&fn) noexcept {
    if (src.isErr()) {
      auto data = std::get<E>(std::move(src.data));
      return Result<T, F>::Err(fn(std::move(data)));
    } else {
      auto data = std::get<T>(std::move(src.data));
      return Result<T, F>::Ok(std::move(data));
    }
  }

  /**
   * @brief Compute new Result using the given function if `src` is Ok,
   * or type-cast `src` if it is Err
   * 
   * This monad computes a `Result<U, E>` from `Result<T, E>`
   * using `Result<U, E> fn(&&T)`
   * * If the Result was Ok, return the Result obtained by applying `fn`
   * to the contained value
   * * If the Result was Err, type-cast a new Err Result without touching the
   * contained error
   * 
   * @tparam U Ok type from return type of `fn`
   * @param src Result to compute new Result from
   * @param fn Function used to map the value
   * @return constexpr Result<U, E> See the description
   */
  template <typename U>
  constexpr static Result<U, E>
  andThen(Result &&src, std::function<Result<U, E>(T &&)> &&fn) noexcept {
    if (src.isOk()) {
      auto data = std::get<T>(std::move(src.data));
      return fn(std::move(data));
    } else {
      auto data = std::get<E>(std::move(src.data));
      return Result<U, E>::Err(std::move(data));
    }
  }

  /**
   * @brief Compute new Result using the given function if `src` is Err,
   * or type-cast `src` if it is Ok
   * 
   * This monad computes a `Result<T, F>` from `Result<T, E>` 
   * using `Result<T, F> fn(&&E)`
   * * If the Result was Err, return the Result obtained by applying `fn`
   * to the contained error
   * * If the Result was Ok, type-cast a new Ok Result without touching the
   * contained value
   * 
   * @tparam F Err type from return type of `fn`
   * @param src Result to compute new Result from
   * @param fn Function used to map the error
   * @return constexpr Result<T, F> See the description
   */
  template <typename F>
  constexpr static Result<T, F>
  orElse(Result &&src, std::function<Result<T, F>(E &&)> &&fn) noexcept {
    if (src.isErr()) {
      auto data = std::get<E>(std::move(src.data));
      return fn(std::move(data));
    } else {
      auto data = std::get<T>(std::move(src.data));
      return Result<T, F>::Ok(std::move(data));
    }
  }

  /**
   * @brief Compute new value from Result of any state using two given functions
   * 
   * This monad computes a `U` from `Result<T, E>`
   * using two functions `U fn(&&T)` and `U fb(&&E)`
   * * If the Result was Ok, return the output obtained by applying `fn`
   * to the contained value
   * * If the Result was Err, return the output obtained by applying `fb`
   * to the contained error
   * 
   * @tparam U Return type of `fb` and `fn`
   * @param src Result to compute output from
   * @param fb Function used to map the error
   * @param fn Function used to map the value
   * @return constexpr U See the description
   */
  template <typename U>
  constexpr static U mapOrElse(Result &&src, std::function<U(E &&)> &&fb,
                               std::function<U(T &&)> &&fn) noexcept {
    if (src.isOk()) {
      auto data = std::get<T>(std::move(src.data));
      return fn(std::move(data));
    } else {
      auto data = std::get<E>(std::move(src.data));
      return fb(std::move(data));
    }
  }

  /**
   * @brief Extract the output from Ok Result
   * 
   * This monad extracts the value from `Result<T, E>`
   * that is expected to be Ok. You should make sure that the Result actually
   * is Ok prior to using this monad. One way to achieve it is using `isOk()`.
   * Exception will be thrown upon trying to unwrap the Err Result.
   * 
   * @param src Result to extract output from
   * @return constexpr T Contained output
   * @throws BadUnwrapError If `src` is Ok Result
   */
  constexpr static T unwrap(Result &&src) {
    if (src.isErr()) {
      throw BadUnwrapError();
    }
    return std::get<T>(std::move(src.data));
  }

/**
 * @brief Extract the error from Err Result
 * 
 * This monad extracts the error from `Result<T, E>`
 * that is expected to be Err. You should make sure that the Result actually
 * is Err prior to using this monad. One way to achieve it is using `isErr()`.
 * Exception will be thrown upon trying to inspect the Ok Result.
 * 
 * @param src Result to extract error from
 * @return constexpr E Contained error
 * @throws BadInspectError If `src` is Ok Result
 */
  constexpr static E inspect(Result &&src) {
    if (src.isOk()) {
      throw BadInspectError();
    }
    return std::get<E>(std::move(src.data));
  }

/**
 * @brief Extract the output from Ok Result, or compute one from the error in
 * Err Result using the given function
 * 
 * This monad computes a `T` from `Result<T, E>`
 * using `T fn(&&E)`
 * * If the Result was Ok, simply extract the contained value
 * * If the Result was Err, return the output obtained by applying `fn`
 * to the contained error
 * 
 * @param src Result to compute output from
 * @param fn Function used to map the error
 * @return constexpr T See the description
 */
  constexpr static T unwrapOrElse(Result &&src,
                                  std::function<T(E &&)> &&fn) noexcept {
    if (src.isOk()) {
      return std::get<T>(std::move(src.data));
    } else {
      auto data = std::get<E>(std::move(src.data));
      return fn(std::move(data));
    }
  }
};
} // namespace result

template <typename T, typename E> using Result = result::Result<T, E>;
#endif // SC_RESULT_H
