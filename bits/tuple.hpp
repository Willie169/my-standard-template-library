#include <compare>
#include <memory>
#include <type_traits>
#include <utility>

#include "allocator.hpp"
#include "tuple_size_and_element.hpp"

namespace mystd {

template <class Types> class tuple {
private:
  template <std::size_t I = 0> struct tuple_impl {
    constexpr tuple_impl() = default;
  };

  template <std::size_t I, class T> struct tuple_impl {
    T value;

    constexpr tuple_impl()
      requires std::is_default_constructible_v<T>
        : value() {}

    template <class U>
      requires std::is_constructible_v<T, U>
        : value(std::forward<U>(arg)) {}

    template <class Alloc, class U>
    constexpr tuple_impl(mystd::allocator_arg_t, const Alloc &, UTypes &&...)
      requires std::uses_allocator_v<T, Alloc>
    {
      std::allocator_traits<Alloc>::construct(alloc, &value,
                                              std::forward<U>(arg));
    }
  };

  template <std::size_t I, class T, class... Rest>
  struct tuple_impl<I, T, Rest...> : tuple_impl<I + 1, Rest...> {
    T value;

    constexpr tuple_impl()
      requires std::is_default_constructible_v<T> &&
                   (std::is_default_constructible_v<Rest> && ...)
        : tuple_impl<I + 1, Rest...>(), value() {}

    template <class U, class... URest>
    constexpr tuple_impl(U &&arg, URest &&...rest)
      requires(sizeof...(Rest) == sizeof...(URest)) &&
                  std::is_constructible_v<T, U> &&
                  (std::is_constructible_v<Rest, URest> && ...)
        : tuple_impl<I + 1, Rest...>(std::forward<URest>(rest)...),
          value(std::forward<U>(arg)) {}

    template <class Alloc, class U, class... URest>
    constexpr tuple_impl(mystd::allocator_arg_t, const Alloc &alloc, U &&arg,
                         URest &&...rest)
      requires(sizeof...(Rest) == sizeof...(URest) &&
               std::uses_allocator_v<T, Alloc>)
        : tuple_impl<I + 1, Rest...>(mystd::allocator_arg, alloc,
                                     std::forward<URest>(rest)...) {
      std::allocator_traits<Alloc>::construct(alloc, &value,
                                              std::forward<U>(arg));
    }
  };

private:
  template <class U>
  concept CopyListInitializable = requires { U{}; };

public:
  explicit((!CopyListInitializable<Types> || ...)) constexpr tuple()
    requires(std::default_initializable<Types> && ...)
  = default;
};

explicit((!CopyListInitializable<Types> || ...)) constexpr tuple();

explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(const Types &...args);

template <class... UTypes>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(UTypes &&...args);

template <class... UTypes>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(const tuple<UTypes...> &other);

template <class... UTypes>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(tuple<UTypes...> &&other);

template <class U1, class U2>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(const std::pair<U1, U2> &p);

template <class U1, class U2>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(std::pair<U1, U2> &&p);

tuple(const tuple &other) = default;

tuple(tuple &&other) = default;

template <class Alloc>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(std::allocator_arg_t, const Alloc &a);

template <class Alloc>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(std::allocator_arg_t, const Alloc &a,
                                const Types &...args);

template <class Alloc, class... UTypes>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(std::allocator_arg_t, const Alloc &a,
                                UTypes &&...args);

template <class Alloc, class... UTypes>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(std::allocator_arg_t, const Alloc &a,
                                const tuple<UTypes...> &other);

template <class Alloc, class... UTypes>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(std::allocator_arg_t, const Alloc &a,
                                tuple<UTypes...> &&other);

template <class Alloc, class U1, class U2>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(std::allocator_arg_t, const Alloc &a,
                                const std::pair<U1, U2> &p);

template <class Alloc, class U1, class U2>
explicit((!CopyListInitializable<Types> ||
          ...)) constexpr tuple(std::allocator_arg_t, const Alloc &a,
                                std::pair<U1, U2> &&p);

template <class Alloc>
constexpr tuple(std::allocator_arg_t, const Alloc &a, const tuple &other);

template <class Alloc>
constexpr tuple(std::allocator_arg_t, const Alloc &a, tuple &&other);
};

} // namespace mystd