#pragma once // tuple.hpp

#include <compare>
#include <type_traits>
#include <utility>

#include "allocator.hpp"
#include "ignore.hpp"
#include "swap.hpp"
#include "tuple_size_and_element.hpp"

namespace mystd {

template <class First, class... Rest> struct _first_type {
  using type = First;
};

template <class... Ts> using _first_type_t = typename _first_type<Ts...>::type;

template <typename T> struct _is_tuple : std::false_type {};

template <typename T> inline constexpr bool _is_tuple_v = _is_tuple<T>::value;

template <std::size_t I, class... Types> struct _tuple_impl;

template <std::size_t I> struct _tuple_impl<I> {
  constexpr _tuple_impl() = default;

  template <typename Alloc>
  constexpr _tuple_impl(mystd::allocator_arg_t, const Alloc &) {}

  constexpr _tuple_impl &operator=(const _tuple_impl &) noexcept {
    return *this;
  }

  constexpr _tuple_impl &operator=(_tuple_impl &&) noexcept { return *this; }

  void swap(_tuple_impl &other) {}
};

template <std::size_t I, class Head, class... Tail>
struct _tuple_impl<I, Head, Tail...> : private _tuple_impl<I + 1, Tail...> {
  using base = _tuple_impl<I + 1, Tail...>;
  Head head;

  constexpr _tuple_impl()
    requires(std::is_default_constructible_v<Head> &&
             (std::is_default_constructible_v<Tail> && ...))
      : base(), head() {}

  explicit((!std::is_convertible_v<const Head &, Head> ||
            (!std::is_convertible_v<const Tail &, Tail> ||
             ...))) constexpr _tuple_impl(const Head &arg, const Tail &...args)
    requires std::is_constructible_v<Head, const Head &> &&
                 (std::is_constructible_v<Tail, const Tail &> && ...)
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
  constexpr _tuple_impl(mystd::allocator_arg_t, const Alloc &alloc)
    requires std::is_default_constructible_v<Head> &&
                 (std::is_default_constructible_v<Tail> && ...)
      : base(mystd::allocator_arg, alloc), head() {}

  template <typename Alloc>
  constexpr _tuple_impl(mystd::allocator_arg_t, const Alloc &alloc,
                        const Head &arg, const Tail &...args)
    requires std::is_constructible_v<Head, const Head &> &&
                 (std::is_constructible_v<Tail, const Tail &> && ...)
      : base(mystd::allocator_arg, alloc, args...), head(arg) {}

  template <typename Alloc, class UHead, class... UTail>
  constexpr _tuple_impl(mystd::allocator_arg_t, const Alloc &alloc, UHead &&arg,
                        UTail &&...args)
    requires(sizeof...(Tail) == sizeof...(UTail)) &&
                std::is_constructible_v<Head, UHead> &&
                (std::is_constructible_v<Tail, UTail> && ...)
      : base(mystd::allocator_arg, alloc, std::forward<UTail>(args)...),
        head(std::forward<UHead>(arg)) {}

  template <class... UTypes>
  constexpr _tuple_impl(const _tuple_impl<I, UTypes...> &other)
    requires(sizeof...(Tail) + 1 == sizeof...(UTypes)) &&
                std::is_constructible_v<
                    Head, const typename _tuple_impl<I, UTypes...>::Head &> &&
                std::is_constructible_v<
                    base, const typename _tuple_impl<I, UTypes...>::base &>
      : base(static_cast<const typename _tuple_impl<I, UTypes...>::base &>(
            other)),
        head(other.head) {}

  constexpr _tuple_impl(const _tuple_impl &other)
    requires(std::is_copy_constructible_v<Head> &&
             std::is_copy_constructible_v<base>)
      : base(static_cast<const base &>(other)), head(other.head) {}

  constexpr _tuple_impl(_tuple_impl &&other)
    requires(std::is_move_constructible_v<Head> &&
             std::is_move_constructible_v<base>)
      : base(std::move(static_cast<base &>(other))),
        head(std::move(other.head)) {}

  template <class... UTypes>
  constexpr _tuple_impl(_tuple_impl<I, UTypes...> &&other)
    requires(sizeof...(Tail) + 1 == sizeof...(UTypes)) &&
                std::is_constructible_v<
                    Head, typename _tuple_impl<I, UTypes...>::Head> &&
                std::is_constructible_v<
                    base, typename _tuple_impl<I, UTypes...>::base>
      : base(std::move(
            static_cast<typename _tuple_impl<I, UTypes...>::base &>(other))),
        head(std::move(other.head)) {}

  constexpr _tuple_impl &operator=(const _tuple_impl &other)
    requires std::is_copy_assignable_v<Head> && std::is_copy_assignable_v<base>
  {
    head = other.head;
    base::operator=(other);
    return *this;
  }

  constexpr _tuple_impl &operator=(_tuple_impl &&other) noexcept(
      std::is_nothrow_move_assignable_v<Head> &&
      std::is_nothrow_move_assignable_v<base>)
    requires(std::is_move_assignable_v<Head> && std::is_move_assignable_v<base>)
  {
    head = std::move(other.head);
    base::operator=(std::move(other));
    return *this;
  }

  constexpr void swap(_tuple_impl &other) noexcept(
      std::is_nothrow_swappable_v<Head> &&
      noexcept(std::declval<base &>().swap(std::declval<base &>()))) {
    mystd::swap(head, other.head);
    static_cast<base &>(*this).swap(static_cast<base &>(other));
  }

  template <std::size_t J>
  constexpr auto &get()
    requires(J >= I && J <= I + sizeof...(Tail))
  {
    if constexpr (I == J) {
      return head;
    } else {
      return base::template get<J>();
    }
  }

  template <std::size_t J>
  constexpr const auto &get() const
    requires(J >= I && J <= I + sizeof...(Tail))
  {
    if constexpr (I == J) {
      return head;
    } else {
      return base::template get<J>();
    }
  }
};

template <class... Types> class tuple {
private:
  _tuple_impl<0, Types...> impl;

public:
  constexpr tuple()
    requires(std::is_default_constructible_v<Types> && ...)
      : impl() {}

  explicit((!std::is_convertible_v<const Types &, Types> ||
            ...)) constexpr tuple(const Types &...args)
    requires(sizeof...(Types) >= 1) &&
            (std::is_copy_constructible_v<Types> && ...)
      : impl(args...) {}

  template <class... UTypes>
  explicit((!std::is_convertible_v<UTypes, Types> ||
            ...)) constexpr tuple(UTypes &&...args)
    requires(sizeof...(Types) >= 1) &&
            (sizeof...(Types) == sizeof...(UTypes)) &&
            (std::is_constructible_v<Types, UTypes> && ...) &&
            (sizeof...(Types) > 1 ||
             !mystd::_is_tuple_v<
                 std::remove_cvref_t<_first_type_t<UTypes...>>>) &&
            (sizeof...(Types) > 3 ||
             !std::is_same_v<std::remove_cvref_t<_first_type_t<UTypes...>>,
                             mystd::allocator_arg_t> ||
             std::is_same_v<_first_type_t<Types...>, mystd::allocator_arg_t>)
      : impl(args...) {}

  template <class... UTypes>
  explicit((!std::is_convertible_v<Types, const UTypes &> ||
            ...)) constexpr tuple(const tuple<UTypes...> &other)
    requires(sizeof...(Types) == sizeof...(UTypes)) &&
            (std::is_constructible_v<Types, const UTypes &> && ...) &&
            (!(sizeof...(Types) == 1 &&
               ((std::is_convertible_v<UTypes, Types> || ...) ||
                (std::is_constructible_v<Types, UTypes> || ...) ||
                (std::is_same_v<Types, UTypes> || ...))))
      : impl(other.impl) {}

  template <class... UTypes>
  explicit((!std::is_convertible_v<Types, UTypes &&> ||
            ...)) constexpr tuple(tuple<UTypes...> &&other)
    requires(sizeof...(Types) == sizeof...(UTypes)) &&
            (std::is_constructible_v<Types, UTypes &&> && ...) &&
            (!(sizeof...(Types) == 1 &&
               ((std::is_convertible_v<UTypes, Types> || ...) ||
                (std::is_constructible_v<Types, UTypes> || ...) ||
                (std::is_same_v<Types, UTypes> || ...))))
      : impl(std::move(other.impl)) {}

  template <class U1, class U2>
  explicit((
      !std::is_convertible_v<U1, std::tuple_element_t<0, tuple<Types...>>> ||
      !std::is_convertible_v<
          U2, std::tuple_element_t<
                  1, tuple<Types...>>>)) constexpr tuple(const std::pair<U1, U2>
                                                             &p)
    requires(sizeof...(Types) == 2) &&
            std::is_constructible_v<std::tuple_element_t<0, tuple<Types...>>,
                                    U1> &&
            std::is_constructible_v<std::tuple_element_t<1, tuple<Types...>>,
                                    U2>
      : impl(p.first, p.second) {}

  template <class U1, class U2>
  explicit((
      !std::is_convertible_v<U1 &&, std::tuple_element_t<0, tuple<Types...>>> ||
      !std::is_convertible_v<
          U2 &&, std::tuple_element_t<
                     1, tuple<Types...>>>)) constexpr tuple(std::pair<U1, U2>
                                                                &&p)
    requires(sizeof...(Types) == 2) &&
            std::is_constructible_v<std::tuple_element_t<0, tuple<Types...>>,
                                    U1 &&> &&
            std::is_constructible_v<std::tuple_element_t<1, tuple<Types...>>,
                                    U2 &&>
      : impl(std::move(p.first), std::move(p.second)) {}

  tuple(const tuple &other) : impl(other.impl) {}

  tuple(tuple &&other) : impl(std::move(other.impl)) {}

  template <class Alloc>
  constexpr tuple(mystd::allocator_arg_t, const Alloc &a)
    requires(std::is_default_constructible_v<Types> && ...)
      : impl(mystd::allocator_arg, a) {}

  template <class Alloc>
  explicit((!std::is_convertible_v<const Types &, Types> ||
            ...)) constexpr tuple(mystd::allocator_arg_t, const Alloc &a,
                                  const Types &...args)
    requires(sizeof...(Types) >= 1) &&
            (std::is_copy_constructible_v<Types> && ...)
      : impl(mystd::allocator_arg, a, args...) {}

  template <class Alloc, class... UTypes>
  explicit((!std::is_convertible_v<UTypes, Types> ||
            ...)) constexpr tuple(mystd::allocator_arg_t, const Alloc &a,
                                  UTypes &&...args)
    requires(sizeof...(Types) >= 1) &&
            (sizeof...(Types) == sizeof...(UTypes)) &&
            (std::is_constructible_v<Types, UTypes> && ...) &&
            (sizeof...(Types) > 1 ||
             !mystd::_is_tuple_v<std::remove_cvref_t<_first_type_t<UTypes...>>>)
      : impl(mystd::allocator_arg, a, args...) {}

  template <class Alloc, class... UTypes>
  explicit((!std::is_convertible_v<Types, const UTypes &> ||
            ...)) constexpr tuple(mystd::allocator_arg_t, const Alloc &a,
                                  const tuple<UTypes...> &other)
    requires(sizeof...(Types) == sizeof...(UTypes)) &&
            (std::is_constructible_v<Types, const UTypes &> && ...) &&
            (!(sizeof...(Types) == 1 &&
               ((std::is_convertible_v<UTypes, Types> || ...) ||
                (std::is_constructible_v<Types, UTypes> || ...) ||
                (std::is_same_v<Types, UTypes> || ...))))
      : impl(mystd::allocator_arg, a, other.impl) {}

  template <class Alloc, class... UTypes>
  explicit((!std::is_convertible_v<Types, UTypes &&> ||
            ...)) constexpr tuple(mystd::allocator_arg_t, const Alloc &a,
                                  tuple<UTypes...> &&other)
    requires(sizeof...(Types) == sizeof...(UTypes)) &&
            (std::is_constructible_v<Types, UTypes &&> && ...) &&
            (!(sizeof...(Types) == 1 &&
               ((std::is_convertible_v<UTypes, Types> || ...) ||
                (std::is_constructible_v<Types, UTypes> || ...) ||
                (std::is_same_v<Types, UTypes> || ...))))
      : impl(mystd::allocator_arg, a, std::move(other.impl)) {}

  template <class Alloc, class U1, class U2>
  explicit((
      !std::is_convertible_v<U1, std::tuple_element_t<0, tuple<Types...>>> ||
      !std::is_convertible_v<
          U2, std::tuple_element_t<
                  1, tuple<Types...>>>)) constexpr tuple(mystd::allocator_arg_t,
                                                         const Alloc &a,
                                                         const std::pair<U1, U2>
                                                             &p)
    requires(sizeof...(Types) == 2) &&
            std::is_constructible_v<std::tuple_element_t<0, tuple<Types...>>,
                                    U1> &&
            std::is_constructible_v<std::tuple_element_t<1, tuple<Types...>>,
                                    U2>
      : impl(mystd::allocator_arg, a, p.first, p.second) {}

  template <class Alloc, class U1, class U2>
  explicit((
      !std::is_convertible_v<U1 &&, std::tuple_element_t<0, tuple<Types...>>> ||
      !std::is_convertible_v<
          U2 &&,
          std::tuple_element_t<
              1, tuple<Types...>>>)) constexpr tuple(mystd::allocator_arg_t,
                                                     const Alloc &a,
                                                     std::pair<U1, U2> &&p)
    requires(sizeof...(Types) == 2) &&
            std::is_constructible_v<std::tuple_element_t<0, tuple<Types...>>,
                                    U1 &&> &&
            std::is_constructible_v<std::tuple_element_t<1, tuple<Types...>>,
                                    U2 &&>
      : impl(mystd::allocator_arg, a, std::move(p.first), std::move(p.second)) {
  }

  template <class Alloc>
  tuple(mystd::allocator_arg_t, const Alloc &a, const tuple &other)
      : impl(mystd::allocator_arg, a, other) {}

  template <class Alloc>
  tuple(mystd::allocator_arg_t, const Alloc &a, tuple &&other)
      : impl(mystd::allocator_arg, a, std::move(other)) {}

  constexpr tuple &operator=(const tuple &other)
    requires(std::is_copy_assignable_v<Types> && ...)
  {
    impl = other.impl;
    return *this;
  }

  constexpr tuple &operator=(tuple &&other) noexcept(
      (std::is_nothrow_move_assignable_v<Types> && ...))
    requires(std::is_move_assignable_v<Types> && ...)
  {
    impl = std::move(other.impl);
    return *this;
  }

  template <class... UTypes>
  constexpr tuple &operator=(const tuple<UTypes...> &other)
    requires(sizeof...(Types) == sizeof...(UTypes)) &&
            (std::is_assignable_v<Types &, const UTypes &> && ...)
  {
    impl = other.impl;
    return *this;
  }

  template <class... UTypes>
  constexpr tuple &operator=(tuple<UTypes...> &&other)
    requires(sizeof...(Types) == sizeof...(UTypes)) &&
            (std::is_assignable_v<Types &, UTypes &&> && ...)
  {
    impl = std::move(other.impl);
    return *this;
  }

  template <class E1, class E2>
  constexpr tuple &operator=(const std::pair<E1, E2> &p)
    requires(sizeof...(Types) == 2) &&
            std::is_assignable_v<std::tuple_element_t<0, tuple<Types...>> &,
                                 const E1 &> &&
            std::is_assignable_v<std::tuple_element_t<1, tuple<Types...>> &,
                                 const E2 &>
  {
    impl.template get<0>() = p.first;
    impl.template get<1>() = p.second;
    return *this;
  }

  template <class E1, class E2>
  constexpr tuple &operator=(std::pair<E1, E2> &&p) noexcept(
      std::is_nothrow_assignable_v<std::tuple_element_t<0, tuple<Types...>> &,
                                   E1 &&> &&
      std::is_nothrow_assignable_v<std::tuple_element_t<1, tuple<Types...>> &,
                                   E2 &&>)
    requires(sizeof...(Types) == 2) &&
            std::is_assignable_v<std::tuple_element_t<0, tuple<Types...>> &,
                                 E1 &&> &&
            std::is_assignable_v<std::tuple_element_t<1, tuple<Types...>> &,
                                 E2 &&>
  {
    impl.template get<0>() = std::move(p.first);
    impl.template get<1>() = std::move(p.second);
    return *this;
  }

  constexpr void swap(tuple &other) noexcept(
      noexcept((std::is_nothrow_swappable_v<Types> && ...))) {
    impl.swap(other.impl);
  }

  template <std::size_t I, class... Ts>
  inline friend constexpr typename tuple_element<I, tuple<Ts...>>::type &
  get(tuple<Ts...> &) noexcept;

  template <std::size_t I, class... Ts>
  inline friend constexpr typename tuple_element<I, tuple<Ts...>>::type &&
  get(tuple<Ts...> &&) noexcept;

  template <std::size_t I, class... Ts>
  inline friend constexpr const typename tuple_element<I, tuple<Ts...>>::type &
  get(const tuple<Ts...> &) noexcept;

  template <std::size_t I, class... Ts>
  inline friend constexpr const typename tuple_element<I, tuple<Ts...>>::type &&
  get(const tuple<Ts...> &&) noexcept;

private:
  template <std::size_t... I>
  static constexpr bool compare_eq(const tuple &a, const tuple &b,
                                   std::index_sequence<I...>);

  template <std::size_t... I>
  static constexpr auto compare_spaceship(const tuple &a, const tuple &b,
                                          std::index_sequence<I...>);

public:
  friend constexpr bool operator==(const tuple &a, const tuple &b)
    requires((std::equality_comparable<Types>) && ...)
  {
    return compare_eq(a, b, std::make_index_sequence<sizeof...(Types)>{});
  }

  friend constexpr auto operator<=>(const tuple &a, const tuple &b)
    requires((std::three_way_comparable<Types>) && ...)
  {
    return compare_spaceship(a, b,
                             std::make_index_sequence<sizeof...(Types)>{});
  }
};

template <class... UTypes> tuple(UTypes...) -> tuple<UTypes...>;

template <class T1, class T2> tuple(std::pair<T1, T2>) -> tuple<T1, T2>;

template <class Alloc, class... UTypes>
tuple(std::allocator_arg_t, Alloc, UTypes...) -> tuple<UTypes...>;

template <class Alloc, class T1, class T2>
tuple(std::allocator_arg_t, Alloc, std::pair<T1, T2>) -> tuple<T1, T2>;

template <class Alloc, class... UTypes>
tuple(std::allocator_arg_t, Alloc, tuple<UTypes...>) -> tuple<UTypes...>;

template <typename... Ts>
struct _is_tuple<mystd::tuple<Ts...>> : std::true_type {};

template <std::size_t I, class... Types>
constexpr typename std::tuple_element<I, mystd::tuple<Types...>>::type &
get(mystd::tuple<Types...> &t) noexcept {
  return t.impl.template get<I>();
}

template <std::size_t I, class... Types>
constexpr typename std::tuple_element<I, mystd::tuple<Types...>>::type &&
get(mystd::tuple<Types...> &&t) noexcept {
  return std::move(t.impl.template get<I>());
}

template <std::size_t I, class... Types>
constexpr const typename std::tuple_element<I, mystd::tuple<Types...>>::type &
get(const mystd::tuple<Types...> &t) noexcept {
  return t.impl.template get<I>();
}

template <std::size_t I, class... Types>
constexpr const typename std::tuple_element<I, mystd::tuple<Types...>>::type &&
get(const mystd::tuple<Types...> &&t) noexcept {
  return std::move(t.impl.template get<I>());
}

template <class T, class... Types> struct _count_of_type;

template <class T>
struct _count_of_type<T> : std::integral_constant<std::size_t, 0> {};

template <class T, class Head, class... Tail>
struct _count_of_type<T, Head, Tail...>
    : std::integral_constant<std::size_t,
                             (std::is_same_v<T, Head> ? 1 : 0) +
                                 _count_of_type<T, Tail...>::value> {};

template <class T, class... Types>
inline constexpr std::size_t _count_of_type_v =
    _count_of_type<T, Types...>::value;

template <class T, class... Types> struct _index_of_type;

template <class T, class... Tail>
struct _index_of_type<T, T, Tail...> : std::integral_constant<std::size_t, 0> {
};

template <class T, class Head, class... Tail>
struct _index_of_type<T, Head, Tail...>
    : std::integral_constant<std::size_t,
                             1 + _index_of_type<T, Tail...>::value> {};

template <class T> struct _index_of_type<T> {};

template <class T, class... Types>
inline constexpr std::size_t _index_of_type_v =
    _index_of_type<T, Types...>::value;

template <class T, class... Types>
constexpr T &get(mystd::tuple<Types...> &t) noexcept
  requires(_count_of_type_v<T, Types...> == 1)
{
  return get<_index_of_type_v<T, Types...>>(t);
}

template <class T, class... Types>
constexpr T &&get(mystd::tuple<Types...> &&t) noexcept
  requires(_count_of_type_v<T, Types...> == 1)
{
  return get<_index_of_type_v<T, Types...>>(std::move(t));
}

template <class T, class... Types>
constexpr const T &get(const mystd::tuple<Types...> &t) noexcept
  requires(_count_of_type_v<T, Types...> == 1)
{
  return get<_index_of_type_v<T, Types...>>(t);
}

template <class T, class... Types>
constexpr const T &&get(const mystd::tuple<Types...> &&t) noexcept
  requires(_count_of_type_v<T, Types...> == 1)
{
  return get<_index_of_type_v<T, Types...>>(std::move(t));
}

template <class... Types>
template <std::size_t... I>
constexpr bool tuple<Types...>::compare_eq(const tuple &a, const tuple &b,
                                           std::index_sequence<I...>) {
  return ((mystd::get<I>(a) == mystd::get<I>(b)) && ...);
}

template <class... Types>
template <std::size_t... I>
constexpr auto tuple<Types...>::compare_spaceship(const tuple &a,
                                                  const tuple &b,
                                                  std::index_sequence<I...>) {
  using common_ordering =
      std::common_type_t<decltype(mystd::get<I>(a) <=> mystd::get<I>(b))...>;
  common_ordering result = common_ordering::equivalent;
  ((result == common_ordering::equivalent
        ? (result = mystd::get<I>(a) <=> mystd::get<I>(b))
        : result),
   ...);
  return result;
}

template <class... Types> constexpr auto make_tuple(Types &&...args) {
  return mystd::tuple<typename std::decay<Types>::type...>(
      std::forward<Types>(args)...);
}

template <class... Types>
constexpr mystd::tuple<Types &...> tie(Types &...args) noexcept {
  return mystd::tuple<Types &...>(args...);
}

template <class... Types>
constexpr tuple<Types &&...> forward_as_tuple(Types &&...args) noexcept {
  return tuple<Types &&...>(std::forward<Types>(args)...);
}

template <class... Tuples> struct _tuple_cat_type;

template <> struct _tuple_cat_type<> {
  using type = mystd::tuple<>;
};

template <class... Ts> struct _tuple_cat_type<mystd::tuple<Ts...>> {
  using type = mystd::tuple<Ts...>;
};

template <class... Ts1, class... Ts2, class... Rest>
struct _tuple_cat_type<mystd::tuple<Ts1...>, mystd::tuple<Ts2...>, Rest...> {
  using type =
      typename _tuple_cat_type<mystd::tuple<Ts1..., Ts2...>, Rest...>::type;
};

template <class... Tuples>
using _tuple_cat_type_t = typename _tuple_cat_type<Tuples...>::type;

template <class T1, class T2, std::size_t... I1, std::size_t... I2>
constexpr auto _tuple_cat_two_impl(T1 &&t1, T2 &&t2, std::index_sequence<I1...>,
                                   std::index_sequence<I2...>) {
  return mystd::tuple{mystd::get<I1>(std::forward<T1>(t1))...,
                      mystd::get<I2>(std::forward<T2>(t2))...};
}

template <class T1, class T2> constexpr auto _tuple_cat_two(T1 &&t1, T2 &&t2) {
  return _tuple_cat_two_impl(
      std::forward<T1>(t1), std::forward<T2>(t2),
      std::make_index_sequence<
          std::tuple_size<std::remove_reference_t<T1>>::value>{},
      std::make_index_sequence<
          std::tuple_size<std::remove_reference_t<T2>>::value>{});
}

template <class T> constexpr auto _tuple_cat_impl(T &&t) {
  return std::forward<T>(t);
}

template <class T1, class T2, class... Rest>
constexpr auto _tuple_cat_impl(T1 &&t1, T2 &&t2, Rest &&...rest) {
  auto first = _tuple_cat_two(std::forward<T1>(t1), std::forward<T2>(t2));
  return _tuple_cat_impl(std::move(first), std::forward<Rest>(rest)...);
}

template <class... Tuples> constexpr auto tuple_cat(Tuples &&...tpls) {
  return _tuple_cat_impl(std::forward<Tuples>(tpls)...);
}

template <class... Types>
constexpr void
swap(mystd::tuple<Types...> &lhs,
     mystd::tuple<Types...> &rhs) noexcept(noexcept(lhs.swap(rhs))) {
  lhs.swap(rhs);
}

template <class... Types, class Alloc>
struct uses_allocator<std::tuple<Types...>, Alloc> : std::true_type {};

} // namespace mystd

namespace std {

template <class... Types>
struct tuple_size<mystd::tuple<Types...>>
    : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <std::size_t I, class Head, class... Tail>
struct tuple_element<I, mystd::tuple<Head, Tail...>>
    : std::tuple_element<I - 1, mystd::tuple<Tail...>> {
  static_assert(I < sizeof...(Tail) + 1, "Index out of bounds");
};

template <class Head, class... Tail>
struct tuple_element<0, mystd::tuple<Head, Tail...>> {
  using type = Head;
};

} // namespace std
