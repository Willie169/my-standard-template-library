#pragma once // deque.hpp

#ifndef _MYSTD_DEQUE_GROW
#define _MYSTD_DEQUE_GROW 2
#endif
#ifndef _MYSTD_DEQUE_MIN_MAP_SIZE
#define _MYSTD_DEQUE_MIN_MAP_SIZE 8
#endif
#ifndef _MYSTD_DEQUE_BLOCK_SIZE
#define _MYSTD_DEQUE_BLOCK_SIZE 512
#endif

#include <algorithm>
#include <compare>
#include <cstddef>
#include <cstring>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <memory>
#include <stdexcept>
#include <utility>

#include "algorithm-copy.hpp"
#include "algorithm-remove.hpp"
#include "allocator.hpp"
#include "range-access.hpp"
#include "swap.hpp"

namespace mystd {

template <class T, class Allocator> class _deque_iterator {
public:
  using iterator_category = std::random_access_iterator_tag;
  using value_type = T;
  using difference_type = std::ptrdiff_t;
  using reference = T &;
  using pointer = std::conditional_t<
      std::is_const_v<T>,
      typename mystd::allocator_traits<Allocator>::const_pointer,
      typename mystd::allocator_traits<Allocator>::pointer>;

private:
  const std::size_t per_block = _MYSTD_DEQUE_BLOCK_SIZE / sizeof(T);
  T **node;
  pointer cur;

public:
  constexpr _deque_iterator() noexcept : node(nullptr), cur(nullptr) {}

  constexpr _deque_iterator(T **n, pointer c) noexcept : node(n), cur(c) {}

  constexpr _deque_iterator(
      const _deque_iterator<std::remove_const_t<T>, Allocator> &other) noexcept
      : node(other.node), cur(other.cur) {}

  constexpr reference operator*() const noexcept { return *cur; }
  constexpr pointer operator->() const noexcept { return cur; }

  constexpr _deque_iterator &operator++() noexcept {
    ++cur;
    if (cur == *node + per_block) {
      ++node;
      cur = *node;
    }
    return *this;
  }

  constexpr _deque_iterator operator++(int) noexcept {
    _deque_iterator tmp = *this;
    ++(*this);
    return tmp;
  }

  constexpr _deque_iterator &operator--() noexcept {
    if (cur == *node) {
      --node;
      cur = *node + per_block - 1;
    } else {
      --cur;
    }
    return *this;
  }

  constexpr _deque_iterator operator--(int) noexcept {
    _deque_iterator tmp = *this;
    --(*this);
    return tmp;
  }

  constexpr _deque_iterator &operator+=(difference_type n) noexcept {
    difference_type offset = (cur - *node) + n;
    if (offset >= 0 && offset < per_block) {
      cur = *node + offset;
    } else {
      difference_type block_shift = offset / per_block;
      if (offset < 0 && offset % per_block)
        block_shift--;
      node += block_shift;
      cur = *node + (offset - block_shift * per_block);
    }
    return *this;
  }

  constexpr _deque_iterator operator+(difference_type n) const noexcept {
    _deque_iterator tmp = *this;
    return tmp += n;
  }

  constexpr _deque_iterator &operator-=(difference_type n) noexcept {
    return *this += -n;
  }

  constexpr _deque_iterator operator-(difference_type n) const noexcept {
    _deque_iterator tmp = *this;
    return tmp -= n;
  }

  constexpr difference_type
  operator-(const _deque_iterator &other) const noexcept {
    return per_block * (node - other.node) + (cur - *node) -
           (other.cur - *(other.node));
  }

  constexpr auto operator<=>(const _deque_iterator &other) const noexcept {
    if (node != other.node)
      return node <=> other.node;
    return cur <=> other.cur;
  }

  constexpr bool operator==(const _deque_iterator &other) const noexcept {
    return cur == other.cur;
  }

  template <class, class> friend class deque;
};

template <class T, class Allocator = mystd::allocator<T>>
  requires std::is_same_v<T, typename Allocator::value_type>
class deque {
public:
  using value_type = T;
  using allocator_type = Allocator;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using reference = T &;
  using const_reference = const T &;
  using pointer = typename mystd::allocator_traits<Allocator>::pointer;
  using const_pointer =
      typename mystd::allocator_traits<Allocator>::const_pointer;
  using iterator = _deque_iterator<T, Allocator>;
  using const_iterator = _deque_iterator<const T, Allocator>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

private:
  [[no_unique_address]] Allocator alloc;
  T **map;
  std::size_t map_size;
  std::size_t sz;
  iterator start;
  iterator finish;
  const std::size_t per_block =
      (_MYSTD_DEQUE_BLOCK_SIZE + sizeof(T) - 1) / sizeof(T);

  constexpr void deallocate() {
    if (map) {
      for (std::size_t i = 0; i < map_size; ++i)
        mystd::allocator_traits<Allocator>::deallocate(alloc, map[i]);
      ::operator delete(map);
      map = nullptr;
    }
    map_size = 0;
    sz = 0;
  }

  constexpr void destroy_deallocate() {
    if (map) {
      if constexpr (!std::is_trivially_destructible_v<T>) {
        for (iterator it = start; it != finish; ++it) {
          mystd::allocator_traits<Allocator>::destroy(alloc, &*it);
        }
      }
      for (std::size_t i = 0; i < map_size; ++i)
        mystd::allocator_traits<Allocator>::deallocate(alloc, map[i]);
      ::operator delete(map);
      map = nullptr;
    }
    map_size = 0;
    sz = 0;
  }

  constexpr void reallocate_map(std::size_t nodes_to_add, bool add_at_front) {
    std::size_t old_num_nodes = finish.node - start.node + 1;
    std::size_t new_map_size =
        (map_size + nodes_to_add) * _MYSTD_DEQUE_GROW + 2;
    T **new_map = static_cast<T **>(::operator new(new_map_size * sizeof(T *)));
    std::size_t new_start = new_map +
                            (new_map_size - old_num_nodes - nodes_to_add) / 2 +
                            (add_at_front ? nodes_to_add : 0);
    mystd::copy(start.node, finish.node + 1, new_start);
    if (map) {
      ::operator delete(map);
    }
    map = new_map;
    map_size = new_map_size;
    start = iterator(new_start, *new_start + (start.cur - *start.node));
    finish = iterator(new_start + old_num_nodes - 1,
                      *(new_start + old_num_nodes - 1) +
                          (finish.cur - *finish.node));
  }

  constexpr void reserve_map_at_front(std::size_t nodes_to_add = 1) {
    if (static_cast<std::size_t>(start.node - map) < nodes_to_add) {
      reallocate_map(nodes_to_add, true);
    }
  }

  constexpr void reserve_map_at_back(std::size_t nodes_to_add = 1) {
    if (map + map_size - finish.node <= nodes_to_add + 1) {
      reallocate_map(nodes_to_add, false);
    }
  }

public:
  constexpr deque() noexcept(noexcept(Allocator()))
      : alloc(Allocator()), map(nullptr), map_size(0), sz(0), start(),
        finish() {}

  explicit constexpr deque(const Allocator &alloc_) noexcept
      : alloc(alloc_), map(nullptr), map_size(0), sz(0), start(), finish() {}

  explicit deque(std::size_t count, const Allocator &alloc_ = Allocator())
      : alloc(alloc_), sz(count) {
    if (count == 0) {
      map = nullptr;
      map_size = 0;
      start = finish = iterator();
      return;
    }
    std::size_t num_blocks = (count + per_block - 1) / per_block;
    map_size = std::max(num_blocks + 2,
                        static_cast<std::size_t>(_MYSTD_DEQUE_MIN_MAP_SIZE));
    map = static_cast<T **>(::operator new(map_size * sizeof(T *)));
    std::size_t start_block = (map_size - num_blocks) / 2;
    for (std::size_t i = 0; i < num_blocks; ++i)
      map[start_block + i] =
          mystd::allocator_traits<Allocator>::allocate(alloc, per_block);
    start = iterator(map + start_block, map[start_block]);
    finish = iterator(map + start_block + num_blocks - 1,
                      map[start_block + num_blocks - 1] +
                          (count - (num_blocks - 1) * per_block));
    if constexpr (!std::is_trivially_default_constructible_v<T>) {
      iterator it = start;
      try {
        for (; it != finish; ++it)
          mystd::allocator_traits<Allocator>::construct(alloc, &*it);
      } catch (...) {
        for (iterator jt = start; jt != it; ++jt)
          mystd::allocator_traits<Allocator>::destroy(alloc, &*jt);
        deallocate();
        throw;
      }
    }
  }

  constexpr deque(std::size_t count, const T &value,
                  const Allocator &alloc_ = Allocator())
      : alloc(alloc_), sz(count) {
    if (count == 0) {
      map = nullptr;
      map_size = 0;
      start = finish = iterator();
      return;
    }
    std::size_t num_blocks = (count + per_block - 1) / per_block;
    map_size = std::max(num_blocks + 2,
                        static_cast<std::size_t>(_MYSTD_DEQUE_MIN_MAP_SIZE));
    map = static_cast<T **>(::operator new(map_size * sizeof(T *)));
    std::size_t start_block = (map_size - num_blocks) / 2;
    for (std::size_t i = 0; i < num_blocks; ++i)
      map[start_block + i] =
          mystd::allocator_traits<Allocator>::allocate(alloc, per_block);
    start = iterator(map + start_block, map[start_block]);
    finish = iterator(map + start_block + num_blocks - 1,
                      map[start_block + num_blocks - 1] +
                          (count - (num_blocks - 1) * per_block));
    if constexpr (!std::is_trivially_default_constructible_v<T>) {
      for (iterator it = start; it != finish; ++it)
        *it = value;
    } else {
      iterator it = start;
      try {
        for (; it != finish; ++it)
          mystd::allocator_traits<Allocator>::construct(alloc, &*it, value);
      } catch (...) {
        for (iterator jt = start; jt != it; ++jt)
          mystd::allocator_traits<Allocator>::destroy(alloc, &*jt);
        deallocate();
        throw;
      }
    }
  }

  template <class InputIt>
  deque(InputIt first, InputIt last, const Allocator &alloc = Allocator());

  deque(const deque &other);

  deque(deque &&other);

  deque(const deque &other, const Allocator &alloc);

  deque(deque &&other, const Allocator &alloc);

  deque(std::initializer_list<T> init, const Allocator &alloc = Allocator());

  ~deque();

  deque &operator=(const deque &other);

  deque &operator=(deque &&other) noexcept(
      std::allocator_traits<Allocator>::is_always_equal::value);

  deque &operator=(std::initializer_list<value_type> ilist);

  void assign(size_type count, const T &value);

  template <class InputIt> void assign(InputIt first, InputIt last);

  void assign(std::initializer_list<T> ilist);

  allocator_type get_allocator() const noexcept;

  T &operator[](std::size_t index);

  const T &operator[](std::size_t index);

  T &at(std::size_t index) {
    if (index >= sz)
      throw std::out_of_range("deque");
    return start[index];
  }

  const T &at(std::size_t index) const {
    if (index >= sz)
      throw std::out_of_range("deque");
    return start[index];
  }

  T &front() { return *start; }
  const T &front() const { return *start; }
  T &back() { return *finish; }
  const T &back() const { return *finish; }

  iterator begin() noexcept { return start; }
  const_iterator begin() const noexcept { return start; }
  const_iterator cbegin() const noexcept { return start; }

  iterator end() noexcept { return finish + 1; }
  const_iterator end() const noexcept { return finish + 1; }
  const_iterator cend() const noexcept { return finish + 1; }

  reverse_iterator rbegin() noexcept { return reverse_iterator(end()); }
  const_reverse_iterator rbegin() const noexcept {
    return const_reverse_iterator(cend());
  }
  const_reverse_iterator crbegin() const noexcept {
    return const_reverse_iterator(cend());
  }

  reverse_iterator rend() noexcept { return reverse_iterator(begin()); }
  const_reverse_iterator rend() const noexcept {
    return const_reverse_iterator(cbegin());
  }
  const_reverse_iterator crend() const noexcept {
    return const_reverse_iterator(cbegin());
  }

  bool empty() const noexcept { return sz == 0; }
  std::size_t size() const noexcept { return sz; }
  std::size_t max_size() const noexcept;
  std::size_t capacity() const noexcept { return map_sz * perb; }

  void shrink_to_fit();

  void clear noexcept;

  iterator insert(const_iterator pos, const T &value);

  iterator insert(const_iterator pos, T &&value);

  iterator insert(const_iterator pos, size_type count, const T &value);

  template <class InputIt>
  iterator insert(const_iterator pos, InputIt first, InputIt last);

  iterator insert(const_iterator pos, std::initializer_list<T> ilist);

  template <class... Args> iterator emplace(const_iterator pos, Args &&...args);

  iterator erase(const_iterator pos);

  iterator erase(const_iterator first, const_iterator last);

  void push_back(const T &value);

  void push_back(T &&value);

  template <class... Args> reference emplace_back(Args &&...args);

  void pop_back();

  void push_front(const T &value);

  void push_front(T &&value);

  template <class... Args> reference emplace_front(Args &&...args);

  void pop_front();

  void resize(size_type count);

  void resize(size_type count, const value_type &value);

  void swap(deque &other) noexcept(
      std::allocator_traits<Allocator>::is_always_equal::value);
};

template <class InputIt,
          class Alloc = std::allocator<
              typename std::iterator_traits<InputIt>::value_type>>
deque(InputIt, InputIt, Alloc = Alloc())
    -> deque<typename std::iterator_traits<InputIt>::value_type, Alloc>;

namespace pmr {
template <class T>
using deque = std::deque<T, std::pmr::polymorphic_allocator<T>>;
}

template <class T, class Alloc>
void swap(mystd::deque<T, Alloc> &lhs,
          mystd::deque<T, Alloc> &rhs) noexcept(noexcept(lhs.swap(rhs)));

template <class T, class Alloc, class U>
typename std::deque<T, Alloc>::size_type erase(mystd::deque<T, Alloc> &c,
                                               const U &value) {
  auto it = mystd::remove(c.begin(), c.end(), value);
  auto r = c.end() - 1;
  c.erase(it, c.end());
  return r;
}

template <class T, class Alloc, class Pred>
typename std::deque<T, Alloc>::size_type erase_if(std::deque<T, Alloc> &c,
                                                  Pred pred) {
  auto it = mystd::remove_if(c.begin(), c.end(), pred);
  auto r = c.end() - 1;
  c.erase(it, c.end());
  return r;
}

} // namespace mystd

template <class T, class Alloc>
bool operator==(const std::deque<T, Alloc> &lhs,
                const std::deque<T, Alloc> &rhs);

template <class T, class Alloc>
std::strong_ordering operator<=>(const std::deque<T, Alloc> &lhs,
                                 const std::deque<T, Alloc> &rhs);
