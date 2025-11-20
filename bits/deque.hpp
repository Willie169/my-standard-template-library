#pragma once

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
#include <type_traits>
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
  const std::size_t per_block =
      _MYSTD_DEQUE_BLOCK_SIZE / sizeof(std::remove_const_t<T>);
  T **block;
  pointer cur;

public:
  constexpr _deque_iterator() noexcept : block(nullptr), cur(nullptr) {}

  constexpr _deque_iterator(T **n, pointer c) noexcept : block(n), cur(c) {}

  template <class U = T,
            std::enable_if_t<std::is_convertible_v<U *, T *>, int> = 0>
  constexpr _deque_iterator(const _deque_iterator<U, Allocator> &other) noexcept
      : block(other.block), cur(other.cur) {}

  constexpr reference operator*() const noexcept { return *cur; }
  constexpr pointer operator->() const noexcept { return cur; }

  constexpr reference operator[](difference_type n) const noexcept {
    return *(*this + n);
  }

  constexpr _deque_iterator &operator++() noexcept {
    ++cur;
    if (cur == *block + per_block) {
      ++block;
      cur = *block;
    }
    return *this;
  }

  constexpr _deque_iterator operator++(int) noexcept {
    _deque_iterator tmp = *this;
    ++(*this);
    return tmp;
  }

  constexpr _deque_iterator &operator--() noexcept {
    if (cur == *block) {
      --block;
      cur = *block + per_block - 1;
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
    difference_type offset = (cur - *block) + n;
    if (offset >= 0 && static_cast<std::size_t>(offset) < per_block) {
      cur = *block + offset;
    } else {
      difference_type block_shift =
          offset / static_cast<difference_type>(per_block);
      if (offset < 0 && offset % static_cast<difference_type>(per_block))
        block_shift--;
      block += block_shift;
      cur = *block +
            (offset - block_shift * static_cast<difference_type>(per_block));
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
    return static_cast<difference_type>(per_block) * (block - other.block) +
           (cur - *block) - (other.cur - *(other.block));
  }

  constexpr auto operator<=>(const _deque_iterator &other) const noexcept {
    if (block != other.block)
      return block <=> other.block;
    return cur <=> other.cur;
  }

  constexpr bool operator==(const _deque_iterator &other) const noexcept {
    return cur == other.cur;
  }

  template <class, class> friend class deque;
  template <class, class> friend class _deque_iterator;
};

template <class T, class Allocator>
constexpr _deque_iterator<T, Allocator>
operator+(typename _deque_iterator<T, Allocator>::difference_type n,
          const _deque_iterator<T, Allocator> &it) noexcept {
  return it + n;
}

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
        if (map[i])
          mystd::allocator_traits<Allocator>::deallocate(alloc, map[i],
                                                         per_block);
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
        if (map[i])
          mystd::allocator_traits<Allocator>::deallocate(alloc, map[i],
                                                         per_block);
      ::operator delete(map);
      map = nullptr;
    }
    map_size = 0;
    sz = 0;
  }

  constexpr void allocate_block(T **block_ptr) {
    *block_ptr = mystd::allocator_traits<Allocator>::allocate(alloc, per_block);
  }

  constexpr void reallocate_map(std::size_t blocks_to_add, bool add_at_front) {
    std::size_t num_blocks = finish.block - start.block + 1;
    std::size_t new_map_size =
        std::max(map_size + blocks_to_add, num_blocks + blocks_to_add) *
            _MYSTD_DEQUE_GROW +
        2;
    T **new_map = static_cast<T **>(::operator new(new_map_size * sizeof(T *)));
    std::size_t new_start_idx =
        (new_map_size - num_blocks - blocks_to_add) / 2 +
        (add_at_front ? blocks_to_add : 0);
    for (std::size_t i = 0; i < new_map_size; ++i)
      new_map[i] = nullptr;
    for (std::size_t i = 0; i < num_blocks; ++i) {
      new_map[new_start_idx + i] = map[start.block - map + i];
    }
    if (map) {
      ::operator delete(map);
    }
    map = new_map;
    map_size = new_map_size;
    start = iterator(map + new_start_idx,
                     map[new_start_idx] + (start.cur - *start.block));
    finish = iterator(map + new_start_idx + num_blocks - 1,
                      map[new_start_idx + num_blocks - 1] +
                          (finish.cur - *finish.block));
  }

  constexpr void reserve_map_at_front(std::size_t blocks_to_add = 1) {
    if (static_cast<std::size_t>(start.block - map) < blocks_to_add) {
      reallocate_map(blocks_to_add, true);
    }
  }

  constexpr void reserve_map_at_back(std::size_t blocks_to_add = 1) {
    if (map_size - static_cast<std::size_t>(finish.block - map) <=
        blocks_to_add) {
      reallocate_map(blocks_to_add, false);
    }
  }

  constexpr void initialize_map() {
    if (!map) {
      map_size = _MYSTD_DEQUE_MIN_MAP_SIZE;
      map = static_cast<T **>(::operator new(map_size * sizeof(T *)));
      std::size_t mid = map_size / 2;
      std::fill(map, map + map_size, nullptr);
      allocate_block(&map[mid]);
      start = finish = iterator(map + mid, map[mid]);
    }
  }

  template <bool Move = false>
  constexpr iterator insert_impl(const_iterator pos, size_type count,
                                 const T &value) {
    if (count == 0)
      return iterator(const_cast<T **>(pos.block), const_cast<T *>(pos.cur));
    difference_type index = pos - start;
    if (index < 0 || static_cast<size_type>(index) > sz) {
      throw std::out_of_range("deque::insert");
    }
    if (static_cast<size_type>(index) == sz) {
      for (size_type i = 0; i < count; ++i) {
        if constexpr (Move) {
          push_back(std::move(const_cast<T &>(value)));
        } else {
          push_back(value);
        }
      }
      return finish - count;
    }
    if (count > per_block / 2) {
      return insert_blocks(pos, count, value);
    }
    for (size_type i = 0; i < count; ++i) {
      insert(start + index + i, value);
    }
    return start + index;
  }

  constexpr iterator insert_blocks(const_iterator pos, size_type count,
                                   const T &value) {
    difference_type index = pos - start;
    size_type old_size = sz;

    for (size_type i = 0; i < count; ++i) {
      push_back(value);
    }

    iterator new_start = start + index;
    iterator new_finish = finish;
    mystd::rotate(new_start, finish - count, finish);
    return new_start;
  }

  template <std::input_iterator InputIt>
  constexpr iterator insert_range_impl(const_iterator pos, InputIt first,
                                       InputIt last, std::input_iterator_tag) {
    difference_type index = pos - start;
    for (; first != last; ++first) {
      insert(start + index, *first);
      ++index;
    }
    return start + index;
  }

  template <std::forward_iterator ForwardIt>
  constexpr iterator insert_range_impl(const_iterator pos, ForwardIt first,
                                       ForwardIt last,
                                       std::forward_iterator_tag) {
    difference_type dist = std::distance(first, last);
    if (dist <= 0)
      return iterator(const_cast<T **>(pos.block), const_cast<T *>(pos.cur));
    difference_type index = pos - start;

    if (static_cast<size_type>(dist) > per_block) {

      for (difference_type i = 0; i < dist; ++i) {
        emplace_back();
      }

      iterator insert_pos = start + index;
      iterator old_end = finish - dist;
      if (insert_pos != old_end) {

        if constexpr (std::is_trivially_copyable_v<T>) {
          std::memmove(insert_pos.cur + dist, insert_pos.cur,
                       (old_end - insert_pos) * sizeof(T));
        } else {
          mystd::copy_backward(insert_pos, old_end, finish);
        }
      }

      mystd::copy(first, last, insert_pos);
      return insert_pos;
    }

    for (; first != last; ++first, ++index) {
      insert(start + index, *first);
    }
    return start + (pos - start);
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

    for (std::size_t i = 0; i < map_size; ++i)
      map[i] = nullptr;
    try {
      for (std::size_t i = 0; i < num_blocks; ++i)
        map[start_block + i] =
            mystd::allocator_traits<Allocator>::allocate(alloc, per_block);
    } catch (...) {
      deallocate();
      throw;
    }
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
        for (pointer i = start.cur; i != it.cur; ++i)
          mystd::allocator_traits<Allocator>::destroy(alloc, i);
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
    for (std::size_t i = 0; i < map_size; ++i)
      map[i] = nullptr;
    try {
      for (std::size_t i = 0; i < num_blocks; ++i)
        map[start_block + i] =
            mystd::allocator_traits<Allocator>::allocate(alloc, per_block);
    } catch (...) {
      deallocate();
      throw;
    }
    start = iterator(map + start_block, map[start_block]);
    finish = iterator(map + start_block + num_blocks - 1,
                      map[start_block + num_blocks - 1] +
                          (count - (num_blocks - 1) * per_block));
    iterator it = start;
    try {
      if constexpr (std::is_trivially_copyable_v<T> &&
                    std::is_trivially_constructible_v<T>) {

        for (std::size_t i = 0; i < num_blocks; ++i) {
          std::size_t block_count = (i == num_blocks - 1)
                                        ? (count - (num_blocks - 1) * per_block)
                                        : per_block;
          std::fill_n(map[start_block + i], block_count, value);
        }
      } else {
        for (; it != finish; ++it)
          mystd::allocator_traits<Allocator>::construct(alloc, &*it, value);
      }
    } catch (...) {
      for (pointer i = start.cur; i != it.cur; ++i)
        mystd::allocator_traits<Allocator>::destroy(alloc, i);
      deallocate();
      throw;
    }
  }

  template <std::input_iterator InputIt>
  constexpr deque(InputIt first, InputIt last,
                  const Allocator &alloc_ = Allocator())
      : alloc(alloc_), map(nullptr), map_size(0), sz(0), start(), finish() {
    if constexpr (std::forward_iterator<InputIt>) {
      if (first == last)
        return;
      std::size_t count = static_cast<std::size_t>(std::distance(first, last));
      if (count == 0)
        return;
      std::size_t num_blocks = (count + per_block - 1) / per_block;
      map_size = std::max(num_blocks + 2,
                          static_cast<std::size_t>(_MYSTD_DEQUE_MIN_MAP_SIZE));
      map = static_cast<T **>(::operator new(map_size * sizeof(T *)));
      std::size_t start_block = (map_size - num_blocks) / 2;
      for (std::size_t i = 0; i < map_size; ++i)
        map[i] = nullptr;
      try {
        for (std::size_t i = 0; i < num_blocks; ++i)
          map[start_block + i] =
              mystd::allocator_traits<Allocator>::allocate(alloc, per_block);
      } catch (...) {
        deallocate();
        throw;
      }
      start = iterator(map + start_block, map[start_block]);
      finish = iterator(map + start_block + num_blocks - 1,
                        map[start_block + num_blocks - 1] +
                            (count - (num_blocks - 1) * per_block));
      sz = count;
      iterator it = start;
      try {
        if constexpr (std::contiguous_iterator<InputIt> &&
                      std::is_trivially_copyable_v<T>) {

          auto *src = std::to_address(first);
          for (std::size_t i = 0; i < num_blocks; ++i) {
            std::size_t block_count =
                (i == num_blocks - 1) ? (count - (num_blocks - 1) * per_block)
                                      : per_block;
            std::memcpy(map[start_block + i], src + i * per_block,
                        block_count * sizeof(T));
          }
        } else {
          for (; first != last; ++first, ++it)
            mystd::allocator_traits<Allocator>::construct(alloc, &*it, *first);
        }
      } catch (...) {
        for (pointer i = start.cur; i != it.cur; ++i)
          mystd::allocator_traits<Allocator>::destroy(alloc, i);
        deallocate();
        throw;
      }
    } else {
      for (; first != last; ++first)
        push_back(*first);
    }
  }

  constexpr deque(const deque &other)
      : alloc(mystd::allocator_traits<
              Allocator>::select_on_container_copy_construction(other.alloc)),
        map(nullptr), map_size(0), sz(0), start(), finish() {
    if (other.sz == 0)
      return;
    std::size_t num_blocks = (other.sz + per_block - 1) / per_block;
    map_size = std::max(num_blocks + 2,
                        static_cast<std::size_t>(_MYSTD_DEQUE_MIN_MAP_SIZE));
    map = static_cast<T **>(::operator new(map_size * sizeof(T *)));
    std::size_t start_block = (map_size - num_blocks) / 2;
    for (std::size_t i = 0; i < map_size; ++i)
      map[i] = nullptr;
    try {
      for (std::size_t i = 0; i < num_blocks; ++i)
        map[start_block + i] =
            mystd::allocator_traits<Allocator>::allocate(alloc, per_block);
    } catch (...) {
      deallocate();
      throw;
    }
    start = iterator(map + start_block, map[start_block]);
    finish = iterator(map + start_block + num_blocks - 1,
                      map[start_block + num_blocks - 1] +
                          (other.sz - (num_blocks - 1) * per_block));
    sz = other.sz;
    iterator it = start;
    const_iterator oit = other.start;
    try {
      if constexpr (std::is_trivially_copyable_v<T>) {

        for (std::size_t i = 0; i < num_blocks; ++i) {
          std::size_t block_count =
              (i == num_blocks - 1) ? (other.sz - (num_blocks - 1) * per_block)
                                    : per_block;
          std::memcpy(map[start_block + i],
                      other.map[other.start.block - other.map + i],
                      block_count * sizeof(T));
        }
      } else {
        for (; oit != other.finish; ++it, ++oit)
          mystd::allocator_traits<Allocator>::construct(alloc, &*it, *oit);
      }
    } catch (...) {
      for (pointer i = start.cur; i != it.cur; ++i)
        mystd::allocator_traits<Allocator>::destroy(alloc, i);
      deallocate();
      throw;
    }
  }

  constexpr deque(deque &&other) noexcept
      : alloc(std::move(other.alloc)), map(other.map), map_size(other.map_size),
        sz(other.sz), start(other.start), finish(other.finish) {
    other.map = nullptr;
    other.map_size = 0;
    other.sz = 0;
    other.start = other.finish = iterator();
  }

  constexpr deque(const deque &other, const Allocator &alloc_)
      : alloc(alloc_), map(nullptr), map_size(0), sz(0), start(), finish() {
    if (other.sz == 0)
      return;
    std::size_t num_blocks = (other.sz + per_block - 1) / per_block;
    map_size = std::max(num_blocks + 2,
                        static_cast<std::size_t>(_MYSTD_DEQUE_MIN_MAP_SIZE));
    map = static_cast<T **>(::operator new(map_size * sizeof(T *)));
    std::size_t start_block = (map_size - num_blocks) / 2;
    for (std::size_t i = 0; i < map_size; ++i)
      map[i] = nullptr;
    try {
      for (std::size_t i = 0; i < num_blocks; ++i)
        map[start_block + i] =
            mystd::allocator_traits<Allocator>::allocate(alloc, per_block);
    } catch (...) {
      deallocate();
      throw;
    }
    start = iterator(map + start_block, map[start_block]);
    finish = iterator(map + start_block + num_blocks - 1,
                      map[start_block + num_blocks - 1] +
                          (other.sz - (num_blocks - 1) * per_block));
    sz = other.sz;
    iterator it = start;
    const_iterator oit = other.start;
    try {
      if constexpr (std::is_trivially_copyable_v<T>) {
        for (std::size_t i = 0; i < num_blocks; ++i) {
          std::size_t block_count =
              (i == num_blocks - 1) ? (other.sz - (num_blocks - 1) * per_block)
                                    : per_block;
          std::memcpy(map[start_block + i],
                      other.map[other.start.block - other.map + i],
                      block_count * sizeof(T));
        }
      } else {
        for (; oit != other.finish; ++it, ++oit)
          mystd::allocator_traits<Allocator>::construct(alloc, &*it, *oit);
      }
    } catch (...) {
      for (pointer i = start.cur; i != it.cur; ++i)
        mystd::allocator_traits<Allocator>::destroy(alloc, i);
      deallocate();
      throw;
    }
  }

  constexpr deque(deque &&other, const Allocator &alloc_)
      : alloc(alloc_), map(nullptr), map_size(0), sz(0), start(), finish() {
    if constexpr (mystd::allocator_traits<Allocator>::is_always_equal::value ||
                  alloc == other.alloc) {
      map = other.map;
      map_size = other.map_size;
      sz = other.sz;
      start = other.start;
      finish = other.finish;
      other.map = nullptr;
      other.map_size = 0;
      other.sz = 0;
      other.start = other.finish = iterator();
    } else {
      if (other.sz == 0)
        return;
      std::size_t num_blocks = (other.sz + per_block - 1) / per_block;
      map_size = std::max(num_blocks + 2,
                          static_cast<std::size_t>(_MYSTD_DEQUE_MIN_MAP_SIZE));
      map = static_cast<T **>(::operator new(map_size * sizeof(T *)));
      std::size_t start_block = (map_size - num_blocks) / 2;
      for (std::size_t i = 0; i < map_size; ++i)
        map[i] = nullptr;
      try {
        for (std::size_t i = 0; i < num_blocks; ++i)
          map[start_block + i] =
              mystd::allocator_traits<Allocator>::allocate(alloc, per_block);
      } catch (...) {
        deallocate();
        throw;
      }
      start = iterator(map + start_block, map[start_block]);
      finish = iterator(map + start_block + num_blocks - 1,
                        map[start_block + num_blocks - 1] +
                            (other.sz - (num_blocks - 1) * per_block));
      sz = other.sz;
      iterator it = start;
      iterator oit = other.start;
      try {
        if constexpr (std::is_trivially_copyable_v<T>) {
          for (std::size_t i = 0; i < num_blocks; ++i) {
            std::size_t block_count =
                (i == num_blocks - 1)
                    ? (other.sz - (num_blocks - 1) * per_block)
                    : per_block;
            std::memcpy(map[start_block + i],
                        other.map[oit.block - other.map + i],
                        block_count * sizeof(T));
          }
        } else {
          for (; oit != other.finish; ++it, ++oit)
            mystd::allocator_traits<Allocator>::construct(alloc, &*it,
                                                          std::move(*oit));
        }
      } catch (...) {
        for (pointer i = start.cur; i != it.cur; ++i)
          mystd::allocator_traits<Allocator>::destroy(alloc, i);
        deallocate();
        throw;
      }
    }
  }

  deque(std::initializer_list<T> ilist, const Allocator &alloc_ = Allocator())
      : deque(ilist.begin(), ilist.end(), alloc_) {}

  constexpr ~deque() { destroy_deallocate(); }

  constexpr deque &operator=(const deque &other) {
    if (this != &other) {
      if constexpr (mystd::allocator_traits<Allocator>::
                        propagate_on_container_copy_assignment::value &&
                    alloc != other.alloc) {
        destroy_deallocate();
        alloc = other.alloc;
      }
      assign(other.start, other.finish);
    }
    return *this;
  }

  constexpr deque &operator=(deque &&other) noexcept(
      mystd::allocator_traits<Allocator>::is_always_equal::value) {
    if (this != &other) {
      if constexpr (mystd::allocator_traits<Allocator>::
                        propagate_on_container_move_assignment::value ||
                    mystd::allocator_traits<
                        Allocator>::is_always_equal::value ||
                    alloc == other.alloc) {
        destroy_deallocate();
        if constexpr (mystd::allocator_traits<Allocator>::
                          propagate_on_container_move_assignment::value)
          alloc = std::move(other.alloc);
        map = other.map;
        map_size = other.map_size;
        sz = other.sz;
        start = other.start;
        finish = other.finish;
        other.map = nullptr;
        other.map_size = 0;
        other.sz = 0;
        other.start = other.finish = iterator();
      } else {
        assign(std::make_move_iterator(other.start),
               std::make_move_iterator(other.finish));
      }
    }
    return *this;
  }

  constexpr deque &operator=(std::initializer_list<value_type> ilist) {
    assign(ilist.begin(), ilist.end());
    return *this;
  }

  constexpr void assign(size_type count, const T &value) {
    clear();
    if (count > 0) {
      if constexpr (std::is_trivially_copyable_v<T>) {

        reserve(count);
        std::fill_n(start.cur, count, value);
        finish = start + count;
        sz = count;
      } else {
        for (size_type i = 0; i < count; ++i)
          push_back(value);
      }
    }
  }

  template <std::input_iterator InputIt>
  constexpr void assign(InputIt first, InputIt last) {
    clear();
    if constexpr (std::forward_iterator<InputIt>) {
      std::size_t count = static_cast<std::size_t>(std::distance(first, last));
      if (count > 0) {
        reserve(count);
        mystd::copy(first, last, start);
        finish = start + count;
        sz = count;
      }
    } else {
      for (; first != last; ++first)
        push_back(*first);
    }
  }

  constexpr void assign(std::initializer_list<T> ilist) {
    assign(ilist.begin(), ilist.end());
  }

  constexpr allocator_type get_allocator() const noexcept { return alloc; }

  constexpr T &operator[](std::size_t index) { return start[index]; }
  constexpr const T &operator[](std::size_t index) const {
    return start[index];
  }

  constexpr T &at(std::size_t index) {
    if (index >= sz)
      throw std::out_of_range("deque");
    return start[index];
  }

  constexpr const T &at(std::size_t index) const {
    if (index >= sz)
      throw std::out_of_range("deque");
    return start[index];
  }

  constexpr T &front() { return *start; }
  constexpr const T &front() const { return *start; }

  constexpr T &back() {
    iterator tmp = finish;
    --tmp;
    return *tmp;
  }

  constexpr const T &back() const {
    const_iterator tmp = finish;
    --tmp;
    return *tmp;
  }

  constexpr iterator begin() noexcept { return start; }
  constexpr const_iterator begin() const noexcept { return start; }
  constexpr const_iterator cbegin() const noexcept { return start; }

  constexpr iterator end() noexcept { return finish; }
  constexpr const_iterator end() const noexcept { return finish; }
  constexpr const_iterator cend() const noexcept { return finish; }

  constexpr reverse_iterator rbegin() noexcept {
    return reverse_iterator(end());
  }
  constexpr const_reverse_iterator rbegin() const noexcept {
    return const_reverse_iterator(cend());
  }
  constexpr const_reverse_iterator crbegin() const noexcept {
    return const_reverse_iterator(cend());
  }

  constexpr reverse_iterator rend() noexcept {
    return reverse_iterator(begin());
  }
  constexpr const_reverse_iterator rend() const noexcept {
    return const_reverse_iterator(cbegin());
  }
  constexpr const_reverse_iterator crend() const noexcept {
    return const_reverse_iterator(cbegin());
  }

  constexpr bool empty() const noexcept { return sz == 0; }
  constexpr std::size_t size() const noexcept { return sz; }

  constexpr std::size_t max_size() const noexcept {
    return std::min(
        mystd::allocator_traits<Allocator>::max_size(alloc),
        static_cast<std::size_t>(std::numeric_limits<std::ptrdiff_t>::max()));
  }

  constexpr void reserve(size_type new_cap) {
    if (new_cap > max_size())
      throw std::length_error("deque::reserve");
    if (new_cap <= sz)
      return;
  }

  constexpr void shrink_to_fit() {
    std::size_t new_map_size = finish.block - start.block + 1;
    if (new_map_size == map_size)
      return;
    T **new_map = static_cast<T **>(::operator new(new_map_size * sizeof(T *)));
    mystd::copy(start.block, finish.block + 1, new_map);

    for (std::size_t i = 0; i < map_size; ++i) {
      if (i < static_cast<std::size_t>(start.block - map) ||
          i > static_cast<std::size_t>(finish.block - map)) {
        if (map[i]) {
          mystd::allocator_traits<Allocator>::deallocate(alloc, map[i],
                                                         per_block);
        }
      }
    }
    ::operator delete(map);
    map = new_map;
    map_size = new_map_size;
    start = iterator(map, *map + (start.cur - *start.block));
    finish = iterator(map + new_map_size - 1,
                      map[new_map_size - 1] + (finish.cur - *finish.block));
  }

  constexpr void clear() noexcept {
    if constexpr (!std::is_trivially_destructible_v<T>) {
      for (pointer i = start.cur; i != finish.cur; ++i)
        mystd::allocator_traits<Allocator>::destroy(alloc, i);
    }
    sz = 0;
    if (map && map_size > 0) {
      std::size_t mid = map_size / 2;
      if (!map[mid])
        allocate_block(&map[mid]);
      start = finish = iterator(map + mid, map[mid]);
    }
  }

  constexpr iterator insert(const_iterator pos, const T &value) {
    return emplace(pos, value);
  }

  constexpr iterator insert(const_iterator pos, T &&value) {
    return emplace(pos, std::move(value));
  }

  constexpr iterator insert(const_iterator pos, size_type count,
                            const T &value) {
    return insert_impl(pos, count, value);
  }

  template <std::input_iterator InputIt>
  constexpr iterator insert(const_iterator pos, InputIt first, InputIt last) {
    return insert_range_impl(
        pos, first, last,
        typename std::iterator_traits<InputIt>::iterator_category{});
  }

  constexpr iterator insert(const_iterator pos,
                            std::initializer_list<T> ilist) {
    return insert(pos, ilist.begin(), ilist.end());
  }

  template <class... Args>
  constexpr iterator emplace(const_iterator pos, Args &&...args) {
    if (pos.cur == start.cur) {
      emplace_front(std::forward<Args>(args)...);
      return start;
    } else if (pos.cur == finish.cur) {
      emplace_back(std::forward<Args>(args)...);
      iterator tmp = finish;
      --tmp;
      return tmp;
    } else {
      difference_type index = pos - start;
      if (static_cast<size_type>(index) < sz / 2) {
        emplace_front(std::forward<Args>(args)...);
        iterator old_start = start;
        ++old_start;
        iterator pos_iter = start + index;
        if constexpr (std::is_trivially_copyable_v<T>) {
          std::memmove(&*old_start, &*(old_start + 1), (index - 1) * sizeof(T));
        } else {
          mystd::copy(old_start + 1, pos_iter + 1, old_start);
        }
        *pos_iter = T(std::forward<Args>(args)...);
        return pos_iter;
      } else {
        emplace_back(std::forward<Args>(args)...);
        iterator pos_iter = start + index;
        if constexpr (std::is_trivially_copyable_v<T>) {
          std::memmove(&*(pos_iter + 1), &*pos_iter,
                       (sz - index - 1) * sizeof(T));
        } else {
          mystd::copy_backward(pos_iter, finish - 2, finish - 1);
        }
        *pos_iter = T(std::forward<Args>(args)...);
        return pos_iter;
      }
    }
  }

  constexpr iterator erase(const_iterator pos) { return erase(pos, pos + 1); }

  constexpr iterator erase(const_iterator first, const_iterator last) {
    if (first == last)
      return iterator(const_cast<T **>(first.block),
                      const_cast<T *>(first.cur));
    difference_type n = last - first;
    difference_type elems_before = first - start;
    if (static_cast<size_type>(elems_before) < (sz - n) / 2) {

      if (elems_before > 0) {
        if constexpr (std::is_trivially_copyable_v<T>) {
          std::memmove(&*(start + n), &*start, elems_before * sizeof(T));
        } else {
          mystd::copy_backward(start, start + elems_before,
                               start + elems_before + n);
          for (std::size_t i = 0; i < n; ++i)
            mystd::allocator_traits<Allocator>::destroy(alloc, start.cur + i);
        }
      }
      start += n;
    } else {

      if (sz - elems_before - n > 0) {
        if constexpr (std::is_trivially_copyable_v<T>) {
          std::memmove(&*(start + elems_before), &*last,
                       (sz - elems_before - n) * sizeof(T));
        } else {
          mystd::copy(start + elems_before + n, finish, start + elems_before);
          for (std::size_t i = n; i > 0; --i)
            mystd::allocator_traits<Allocator>::destroy(alloc, finish.cur - i);
        }
      }
      finish -= n;
    }
    sz -= n;
    return start + elems_before;
  }

  constexpr void push_back(const T &value) { emplace_back(value); }

  constexpr void push_back(T &&value) { emplace_back(std::move(value)); }

  template <class... Args> constexpr reference emplace_back(Args &&...args) {
    if (sz == 0)
      initialize_map();
    if (finish.cur != *finish.block + per_block - 1) {
      mystd::allocator_traits<Allocator>::construct(
          alloc, finish.cur, std::forward<Args>(args)...);
      T &result = *finish.cur;
      ++finish.cur;
      ++sz;
      return result;
    } else {
      reserve_map_at_back();
      allocate_block(finish.block + 1);
      mystd::allocator_traits<Allocator>::construct(
          alloc, *finish.block + per_block - 1, std::forward<Args>(args)...);
      T &result = *(*finish.block + per_block - 1);
      ++finish.block;
      finish.cur = *finish.block;
      ++sz;
      return result;
    }
  }

  constexpr void pop_back() {
    if (sz == 0)
      return;
    if (finish.cur != *finish.block) {
      --finish.cur;
      mystd::allocator_traits<Allocator>::destroy(alloc, finish.cur);
    } else {
      --finish.block;
      finish.cur = *finish.block + per_block - 1;
      mystd::allocator_traits<Allocator>::destroy(alloc, finish.cur);
    }
    --sz;
  }

  constexpr void push_front(const T &value) { emplace_front(value); }

  constexpr void push_front(T &&value) { emplace_front(std::move(value)); }

  template <class... Args> constexpr reference emplace_front(Args &&...args) {
    if (sz == 0)
      initialize_map();
    if (start.cur != *start.block) {
      --start.cur;
      mystd::allocator_traits<Allocator>::construct(
          alloc, start.cur, std::forward<Args>(args)...);
    } else {
      reserve_map_at_front();
      allocate_block(start.block - 1);
      --start.block;
      start.cur = *start.block + per_block - 1;
      mystd::allocator_traits<Allocator>::construct(
          alloc, start.cur, std::forward<Args>(args)...);
    }
    ++sz;
    return *start;
  }

  constexpr void pop_front() {
    if (sz == 0)
      return;
    mystd::allocator_traits<Allocator>::destroy(alloc, start.cur);
    if (start.cur != *start.block + per_block - 1) {
      ++start.cur;
    } else {
      ++start.block;
      start.cur = *start.block;
    }
    --sz;
  }

  constexpr void resize(size_type count) {
    if (count > sz) {
      if constexpr (std::is_default_constructible_v<T>) {
        for (size_type i = sz; i < count; ++i) {
          emplace_back();
        }
      } else {
        throw std::logic_error(
            "deque::resize: T must be default constructible");
      }
    } else if (count < sz) {
      for (size_type i = count; i < sz; ++i) {
        pop_back();
      }
    }
  }

  constexpr void resize(size_type count, const value_type &value) {
    if (count > sz) {
      for (size_type i = sz; i < count; ++i) {
        push_back(value);
      }
    } else if (count < sz) {
      for (size_type i = count; i < sz; ++i) {
        pop_back();
      }
    }
  }

  constexpr void swap(deque &other) noexcept(
      mystd::allocator_traits<Allocator>::is_always_equal::value) {
    if constexpr (mystd::allocator_traits<
                      Allocator>::propagate_on_container_swap::value)
      mystd::swap(alloc, other.alloc);
    mystd::swap(map, other.map);
    mystd::swap(map_size, other.map_size);
    mystd::swap(sz, other.sz);
    mystd::swap(start, other.start);
    mystd::swap(finish, other.finish);
  }
};

template <class InputIt,
          class Alloc = mystd::allocator<
              typename std::iterator_traits<InputIt>::value_type>>
deque(InputIt, InputIt, Alloc = Alloc())
    -> deque<typename std::iterator_traits<InputIt>::value_type, Alloc>;

namespace pmr {
template <class T>
using deque = mystd::deque<T, std::pmr::polymorphic_allocator<T>>;
}

template <class T, class Alloc>
constexpr void
swap(mystd::deque<T, Alloc> &lhs,
     mystd::deque<T, Alloc> &rhs) noexcept(noexcept(lhs.swap(rhs))) {
  lhs.swap(rhs);
}

template <class T, class Alloc, class U>
constexpr typename mystd::deque<T, Alloc>::size_type
erase(mystd::deque<T, Alloc> &c, const U &value) {
  auto it = mystd::remove(c.begin(), c.end(), value);
  auto r = c.end() - it;
  c.erase(it, c.end());
  return r;
}

template <class T, class Alloc, class Pred>
constexpr typename mystd::deque<T, Alloc>::size_type
erase_if(mystd::deque<T, Alloc> &c, Pred pred) {
  auto it = mystd::remove_if(c.begin(), c.end(), pred);
  auto r = c.end() - it;
  c.erase(it, c.end());
  return r;
}

} // namespace mystd

template <class T, class Alloc>
constexpr bool operator==(const mystd::deque<T, Alloc> &lhs,
                          const mystd::deque<T, Alloc> &rhs) {
  if (lhs.size() != rhs.size())
    return false;
  return std::equal(lhs.begin(), lhs.end(), rhs.begin());
}

template <class T, class Alloc>
constexpr std::strong_ordering operator<=>(const mystd::deque<T, Alloc> &lhs,
                                           const mystd::deque<T, Alloc> &rhs) {
  return std::lexicographical_compare_three_way(lhs.begin(), lhs.end(),
                                                rhs.begin(), rhs.end());
}
