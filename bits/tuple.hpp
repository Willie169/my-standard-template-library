#pragma once // tuple.hpp

#include <compare>
#include <memory>
#include <type_traits>
#include <utility>

#include "allocator.hpp"
#include "tuple_size_and_element.hpp"

namespace mystd {

class _tuple_impl;
class tuple;

template <class... Types>
struct tuple_size<mystd::tuple<Types...>>
    : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <std::size_t I, std::size_t J, class Head, class... Tail>
struct tuple_element<I, mystd::_tuple_impl<J, Head, Tail...>>
    : tuple_element<I, mystd::_tuple_impl<J + 1, Tail...>> {
  static_assert(I >= J || I < J + sizeof...(Tail) + 1, "Index out of bounds");
};

template <std::size_t I, class Head, class... Tail>
struct tuple_element<I, mystd::_tuple_impl<I, Head, Tail...>> {
  using type = Head;
};

template <std::size_t I, class Head, class... Tail>
struct tuple_element<I, mystd::tuple<Head, Tail...>>
    : mystd::tuple_element<I - 1, mystd::tuple<Tail...>> {
  static_assert(I < sizeof...(Tail) + 1, "Index out of bounds");
};

template <class Head, class... Tail>
struct tuple_element<0, mystd::tuple<Head, Tail...>> {
  using type = Head;
};

template <class First, class... Rest> struct _first_type {
  using type = First;
};

template <class... Ts> using _first_type_t = typename _first_type<Ts...>::type;

template <std::size_t I, class... Types> struct _tuple_impl;

template <std::size_t I> struct _tuple_impl<I> {
  constexpr _tuple_impl() = default;

  template <typename Alloc>
  constexpr _tuple_impl(std::allocator_arg_t, const Alloc &) {}
};

template <std::size_t I, class Head, class... Tail>
struct _tuple_impl<I, Head, Tail...> : private _tuple_impl<I + 1, Tail...> {
private:
  using base = _tuple_impl<I + 1, Tail...>;
  Head head;

public:
  constexpr _tuple_impl()
    requires(std::is_default_constructible_v<Head> &&
             (std::is_default_constructible_v<Tail> && ...))
      : base(), head() {}

  explicit((!std::is_convertible_v<const Head &, Head> ||
            (!std::is_convertible_v<const Tail &, Tail> ||
             ...))) constexpr _tuple_impl(const Head &arg, const Tail &...args)
    requires(std::is_constructible_v<Head, const Head &> &&
             (std::is_constructible_v<Tail, const Tail &> && ...))
      : base(args...), head(arg) {}

  template <class UHead, class... UTail>
  explicit((!std::is_convertible_v<UHead, Head> ||
            (!std::is_convertible_v<UTail, Tail> ||
             ...))) constexpr _tuple_impl(UHead &&arg, UTail &&...args)
    requires(sizeof...(Tail) == sizeof...(UTail)) &&
                std::is_constructible_v<Head, UHead> &&
                (std::is_constructible_v<Tail, UTail> && ...)
      : base(std::forward<UTail>(args)...), head(std::forward<UHead>(arg)) {}

  template <typename Alloc>
  constexpr _tuple_impl(std::allocator_arg_t, const Alloc &alloc)
    requires(std::is_default_constructible_v<Head> &&
             (std::is_default_constructible_v<Tail> && ...))
      : base(std::allocator_arg, alloc), head() {}

  template <typename Alloc>
  constexpr _tuple_impl(std::allocator_arg_t, const Alloc &alloc,
                        const Head &arg, const Tail &...args)
    requires(std::is_constructible_v<Head, const Head &> &&
             (std::is_constructible_v<Tail, const Tail &> && ...))
      : base(std::allocator_arg, alloc, args...), head(arg) {}

  template <typename Alloc, class UHead, class... UTail>
  constexpr _tuple_impl(std::allocator_arg_t, const Alloc &alloc, UHead &&arg,
                        UTail &&...args)
    requires(sizeof...(Tail) == sizeof...(UTail)) &&
                std::is_constructible_v<Head, UHead> &&
                (std::is_constructible_v<Tail, UTail> && ...)
      : base(std::allocator_arg, alloc, std::forward<UTail>(args)...),
        head(std::forward<UHead>(arg)) {}

  template <class... UTypes>
  constexpr _tuple_impl(const _tuple_impl<I, UTypes...> &other)
    requires(sizeof...(Tail) + 1 == sizeof...(UTypes)) &&
                (std::is_constructible_v<Head,
                                         const mystd::tuple_element_t<
                                             I, _tuple_impl<I, UTypes...>> &> &&
                 (std::is_constructible_v<
                      Tail, const mystd::tuple_element_t<
                                I + 1, _tuple_impl<I, UTypes...>> &> &&
                  ...))
      : base(static_cast<const _tuple_impl<I + 1, UTypes...> &>(other)),
        head(other.head) {}

  template <class... UTypes>
  constexpr _tuple_impl(_tuple_impl<I, UTypes...> &&other)
    requires(sizeof...(Tail) + 1 == sizeof...(UTypes)) &&
                (std::is_constructible_v<
                     Head,
                     mystd::tuple_element_t<I, _tuple_impl<I, UTypes...>> &&> &&
                 (std::is_constructible_v<
                      Tail, mystd::tuple_element_t<
                                I + 1, _tuple_impl<I, UTypes...>> &&> &&
                  ...))
      : base(static_cast<_tuple_impl<I + 1, UTypes...> &&>(other)),
        head(std::move(other.head)) {}

  template <std::size_t J, std::size_t K, class... Ts>
  friend constexpr auto &_get_impl(_tuple_impl<K, Ts...> &t);

  template <std::size_t J, std::size_t K, class... Ts>
  friend constexpr const auto &_get_impl(const _tuple_impl<K, Ts...> &t);
};

template <std::size_t I, std::size_t J, class... Types>
constexpr auto &_get_impl(_tuple_impl<I, Types...> &t) {
  if constexpr (I == J) {
    return t.head;
  } else {
    return _get_impl<J>(static_cast<_tuple_impl<I + 1, Types...> &>(t));
  }
}

template <std::size_t I, std::size_t J, class... Types>
constexpr const auto &_get_impl(const _tuple_impl<I, Types...> &t) {
  if constexpr (I == J) {
    return t.head;
  } else {
    return _get_impl<J>(static_cast<const _tuple_impl<I + 1, Types...> &>(t));
  }
}

template <std::size_t I, class Head, class... Tail>
constexpr auto &get(_tuple_impl<0, Head, Tail...> &t) {
  static_assert(I < sizeof...(Tail) + 1, "Index out of bounds");
  return _get_impl<I, 0>(t);
}

template <std::size_t I, class Head, class... Tail>
constexpr const auto &get(const _tuple_impl<0, Head, Tail...> &t) {
  static_assert(I < sizeof...(Tail) + 1, "Index out of bounds");
  return _get_impl<I, 0>(t);
}

template <std::size_t I, class Head, class... Tail>
constexpr auto &&get(_tuple_impl<0, Head, Tail...> &&t) {
  static_assert(I < sizeof...(Tail) + 1, "Index out of bounds");
  return std::move(_get_impl<I, 0>(t));
}

template <std::size_t I, class Head, class... Tail>
constexpr const auto &&get(const _tuple_impl<0, Head, Tail...> &&t) {
  static_assert(I < sizeof...(Tail) + 1, "Index out of bounds");
  return std::move(_get_impl<I, 0>(t));
}

template <class... Types> class tuple {
private:
  _tuple_impl<0, Types...> impl;

public:
  constexpr tuple()
    requires(std::default_constructible<Types> && ...)
      : impl() {}

  explicit((!std::is_convertible_v<const Types &, Types> ||
            ...)) constexpr tuple(const Types &...args)
    requires(sizeof...(Types) >= 1) &&
            (std::is_copy_constructible_v<Types> && ...)
      : impl(args...);

  template <class... UTypes>
  explicit((!std::is_convertible_v<UTypes, Types> ||
            ...)) constexpr tuple(UTypes &&...args)
    requires sizeof
  ...(Types) >= 1 && sizeof...(Types) == sizeof...(UTypes) &&
      (std::is_constructible_v<Types, UTypes> && ...) &&
      (sizeof...(Types) != 1 ||
       !std::is_same_v<std::remove_cvref_t<_first_type_t<UTypes...>>,
                       mystd::tuple>) &&
      (sizeof...(Types) > 3 ||
       !std::is_same_v<std::remove_cvref_t<_first_type_t<UTypes...>>,
                       std::allocator_arg_t> ||
       std::is_same_v<_first_type_t<Types...>, std::allocator_arg_t>)
      : impl(args...);

private:
  template <std::size_t... Is, class... UTypes>
  explicit((!std::is_convertible_v<decltype(std::get<Is>(other)), Types> ||
            ...)) constexpr tuple(const tuple<UTypes...> &other,
                                  std::index_sequence<Is...>)
    requires(sizeof...(Types) == sizeof...(UTypes)) &&
            (std::is_constructible_v<
                 Types, decltype(std::get<Is>(
                            std::forward<decltype(other)>(other)))> &&
             ...) &&
            (!(sizeof...(Types) == 1 &&
               (std::is_convertible_v<decltype(other),
                                      mystd::tuple_element_t<0, tuple>> ||
                std::is_constructible_v<mystd::tuple_element_t<0, tuple>,
                                        decltype(other)> ||
                std::is_same_v<mystd::tuple_element_t<0, tuple>,
                               mystd::tuple_element_t<0, tuple<UTypes...>>>)))
      : impl(std::get<Is>(other)...) {}

  template <std::size_t... Is, class... UTypes>
  explicit((
      !std::is_convertible_v<decltype(std::get<Is>(std::move(other))), Types> ||
      ...)) constexpr tuple(tuple<UTypes...> &&other,
                            std::index_sequence<Is...>)
    requires(sizeof...(Types) == sizeof...(UTypes)) &&
            (std::is_constructible_v<
                 Types, decltype(std::get<Is>(
                            std::forward<decltype(other)>(other)))> &&
             ...) &&
            (!(sizeof...(Types) == 1 &&
               (std::is_convertible_v<decltype(std::move(other)),
                                      mystd::tuple_element_t<0, tuple>> ||
                std::is_constructible_v<mystd::tuple_element_t<0, tuple>,
                                        decltype(std::move(other))> ||
                std::is_same_v<mystd::tuple_element_t<0, tuple>,
                               mystd::tuple_element_t<0, tuple<UTypes...>>>)))
      : impl(std::get<Is>(std::move(other))...) {}

public:
  template <class... UTypes>
  constexpr tuple(const tuple<UTypes...> &other)
      : tuple(other, std::make_index_sequence<sizeof...(Types)>{}) {}

  template <class... UTypes>
  constexpr tuple(tuple<UTypes...> &&other)
      : tuple(std::move(other), std::make_index_sequence<sizeof...(Types)>{}) {}

  template <class U1, class U2>
  explicit(
      (!std::is_convertible_v<const U1 &, mystd::tuple_element_t<0, tuple>> ||
       !std::is_convertible_v<
           const U2 &, mystd::tuple_element_t<
                           1, tuple>>)) constexpr tuple(const std::pair<U1, U2>
                                                            &p)
    requires(sizeof...(Types) == 2) &&
            std::is_constructible_v<mystd::tuple_element_t<0, tuple>,
                                    const U1 &> &&
            std::is_constructible_v<mystd::tuple_element_t<1, tuple>,
                                    const U2 &>
      : impl(std::get<0>(p), std::get<1>(p)) {}

  template <class U1, class U2>
  explicit((!std::is_convertible_v<U1 &&, mystd::tuple_element_t<0, tuple>> ||
            !std::is_convertible_v<
                U2 &&, mystd::tuple_element_t<
                           1, tuple>>)) constexpr tuple(std::pair<U1, U2> &&p)
    requires(sizeof...(Types) == 2) &&
            std::is_constructible_v<mystd::tuple_element_t<0, tuple>, U1 &&> &&
            std::is_constructible_v<mystd::tuple_element_t<1, tuple>, U2 &&>
      : impl(std::get<0>(std::move(p)), std::get<1>(std::move(p))) {}

  tuple(const tuple &other) = default;

  tuple(tuple &&other) = default;

  template <class Alloc> constexpr tuple(std::allocator_arg_t, const Alloc &a);

  template <class Alloc>
  constexpr tuple(std::allocator_arg_t, const Alloc &a, const Types &...args);

  template <class Alloc, class... UTypes>
  constexpr tuple(std::allocator_arg_t, const Alloc &a, UTypes &&...args);

  template <class Alloc, class... UTypes>
  constexpr tuple(std::allocator_arg_t, const Alloc &a,
                  const tuple<UTypes...> &other);

  template <class Alloc, class... UTypes>
  constexpr tuple(std::allocator_arg_t, const Alloc &a,
                  tuple<UTypes...> &&other);

  template <class Alloc, class U1, class U2>
  constexpr tuple(std::allocator_arg_t, const Alloc &a,
                  const std::pair<U1, U2> &p);

  template <class Alloc, class U1, class U2>
  constexpr tuple(std::allocator_arg_t, const Alloc &a, std::pair<U1, U2> &&p);

  template <class Alloc>
  constexpr tuple(std::allocator_arg_t, const Alloc &a, const tuple &other);

  template <class Alloc>
  constexpr tuple(std::allocator_arg_t, const Alloc &a, tuple &&other);
};

template <std::size_t I, class... Types>
constexpr auto &get(tuple<Types...> &t) {
  return get<I>(t.impl);
}

template <std::size_t I, class... Types>
constexpr const auto &get(const tuple<Types...> &t) {
  return get<I>(t.impl);
}

} // namespace mystd