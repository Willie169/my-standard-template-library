#pragma once // tuple_size_and_element.hpp

#include <cstddef>
#include <type_traits>

namespace mystd {

template <class T> struct tuple_size;

template <class T>
struct tuple_size<const T>
    : std::integral_constant<std::size_t, mystd::tuple_size<T>::value> {};

template <class T>
constexpr std::size_t tuple_size_v = mystd::tuple_size<T>::value;

template <std::size_t I, class T> struct tuple_element;

template <std::size_t I, class T> struct tuple_element<I, const T> {
  using type =
      typename std::add_const<typename mystd::tuple_element<I, T>::type>::type;
};

} // namespace mystd
