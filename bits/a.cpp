// tuple_test.cpp
#include <iostream>
#include <string>
#include <utility>
#include <memory>
#include <type_traits>
#include <compare>
#include <cassert>

// Include your tuple implementation
#include "tuple.hpp"

using namespace mystd;

// Test utilities
namespace test_utils {
    int test_count = 0;
    int passed_count = 0;
    
    void reset() {
        test_count = 0;
        passed_count = 0;
    }
    
    void print_stats() {
        std::cout << "Tests passed: " << passed_count << "/" << test_count << "\n";
    }
    
    // Overloaded assert_equal for different types
    template<typename T, typename U>
    void assert_equal(const T& actual, const U& expected, const char* message) {
        ++test_count;
        if (actual == expected) {
            ++passed_count;
            std::cout << "PASS: " << message << "\n";
        } else {
            std::cout << "FAIL: " << message << " (expected: " << expected << ", actual: " << actual << ")\n";
        }
    }
    
    // Specialization for floating point comparison with tolerance
    void assert_equal(double actual, double expected, const char* message, double tolerance = 1e-10) {
        ++test_count;
        if (std::abs(actual - expected) < tolerance) {
            ++passed_count;
            std::cout << "PASS: " << message << "\n";
        } else {
            std::cout << "FAIL: " << message << " (expected: " << expected << ", actual: " << actual << ")\n";
        }
    }
    
    void assert_equal(float actual, float expected, const char* message, float tolerance = 1e-5f) {
        ++test_count;
        if (std::abs(actual - expected) < tolerance) {
            ++passed_count;
            std::cout << "PASS: " << message << "\n";
        } else {
            std::cout << "FAIL: " << message << " (expected: " << expected << ", actual: " << actual << ")\n";
        }
    }
    
    // For C-string comparison
    void assert_equal(const char* actual, const char* expected, const char* message) {
        ++test_count;
        if (std::string(actual) == std::string(expected)) {
            ++passed_count;
            std::cout << "PASS: " << message << "\n";
        } else {
            std::cout << "FAIL: " << message << " (expected: " << expected << ", actual: " << actual << ")\n";
        }
    }
    
    void assert_equal(const char* actual, const std::string& expected, const char* message) {
        assert_equal(actual, expected.c_str(), message);
    }
    
    void assert_equal(const std::string& actual, const char* expected, const char* message) {
        assert_equal(actual.c_str(), expected, message);
    }
    
    void assert_true(bool condition, const char* message) {
        ++test_count;
        if (condition) {
            ++passed_count;
            std::cout << "PASS: " << message << "\n";
        } else {
            std::cout << "FAIL: " << message << "\n";
        }
    }
    
    void assert_false(bool condition, const char* message) {
        assert_true(!condition, message);
    }
}

// Test types
struct NonCopyable {
    int value;
    NonCopyable(int v = 0) : value(v) {}
    NonCopyable(const NonCopyable&) = delete;
    NonCopyable& operator=(const NonCopyable&) = delete;
    NonCopyable(NonCopyable&&) = default;
    NonCopyable& operator=(NonCopyable&&) = default;
    ~NonCopyable() = default;
    
    bool operator==(const NonCopyable& other) const { return value == other.value; }
};

struct NonMovable {
    int value;
    NonMovable(int v = 0) : value(v) {}
    NonMovable(const NonMovable&) = default;
    NonMovable& operator=(const NonMovable&) = default;
    NonMovable(NonMovable&&) = delete;
    NonMovable& operator=(NonMovable&&) = delete;
    ~NonMovable() = default;
    
    bool operator==(const NonMovable& other) const { return value == other.value; }
};

struct ThreeWayComparable {
    int value;
    constexpr ThreeWayComparable(int v) : value(v) {}
    auto operator<=>(const ThreeWayComparable&) const = default;
};

// Test functions
void test_basic_construction() {
    std::cout << "=== Basic Construction Tests ===\n";
    
    // Default construction
    mystd::tuple<int, double, std::string> t1;
    test_utils::assert_equal(mystd::get<0>(t1), 0, "Default construction int");
    test_utils::assert_equal(mystd::get<1>(t1), 0.0, "Default construction double");
    test_utils::assert_equal(mystd::get<2>(t1), std::string(""), "Default construction string");
    
    // Direct construction
    mystd::tuple<int, double, std::string> t2(42, 3.14, "hello");
    test_utils::assert_equal(mystd::get<0>(t2), 42, "Direct construction int");
    test_utils::assert_equal(mystd::get<1>(t2), 3.14, "Direct construction double");
    test_utils::assert_equal(mystd::get<2>(t2), std::string("hello"), "Direct construction string");
    
    // Copy construction
    mystd::tuple<int, std::string> t3(100, "copy");
    mystd::tuple<int, std::string> t4(t3);
    test_utils::assert_equal(mystd::get<0>(t4), 100, "Copy construction int");
    test_utils::assert_equal(mystd::get<1>(t4), "copy", "Copy construction string");
    
    // Move construction
    mystd::tuple<std::string> t5("move");
    mystd::tuple<std::string> t6(std::move(t5));
    test_utils::assert_equal(mystd::get<0>(t6), "move", "Move construction");
    
    std::cout << "\n";
}

void test_element_access() {
    std::cout << "=== Element Access Tests ===\n";
    
    mystd::tuple<int, double, std::string> t(1, 2.5, "three");
    
    // get by index
    test_utils::assert_equal(mystd::get<0>(t), 1, "get<0>");
    test_utils::assert_equal(mystd::get<1>(t), 2.5, "get<1>");
    test_utils::assert_equal(mystd::get<2>(t), std::string("three"), "get<2>");
    
    // get by type
    test_utils::assert_equal(mystd::get<int>(t), 1, "get<int>");
    test_utils::assert_equal(mystd::get<double>(t), 2.5, "get<double>");
    test_utils::assert_equal(mystd::get<std::string>(t), "three", "get<string>");
    
    // const access
    const auto& ct = t;
    test_utils::assert_equal(mystd::get<0>(ct), 1, "const get<0>");
    test_utils::assert_equal(mystd::get<int>(ct), 1, "const get<int>");
    
    // rvalue access
    mystd::tuple<std::string> t2("rvalue");
    std::string s = mystd::get<0>(std::move(t2));
    test_utils::assert_equal(s, "rvalue", "rvalue get");
    
    std::cout << "\n";
}

void test_assignment() {
    std::cout << "=== Assignment Tests ===\n";
    
    // Copy assignment
    mystd::tuple<int, std::string> a(1, "one");
    mystd::tuple<int, std::string> b(2, "two");
    a = b;
    test_utils::assert_equal(mystd::get<0>(a), 2, "Copy assignment int");
    test_utils::assert_equal(mystd::get<1>(a), "two", "Copy assignment string");
    
    // Move assignment
    mystd::tuple<std::string> c("original");
    mystd::tuple<std::string> d("moved");
    c = std::move(d);
    test_utils::assert_equal(mystd::get<0>(c), "moved", "Move assignment");
    
    // Heterogeneous assignment
    mystd::tuple<int, double> e(1, 1.0);
    mystd::tuple<long, float> f(2L, 2.0f);
    e = f;
    test_utils::assert_equal(mystd::get<0>(e), 2, "Heterogeneous assignment int");
    test_utils::assert_equal(mystd::get<1>(e), 2.0, "Heterogeneous assignment double");
    
    std::cout << "\n";
}

void test_comparison() {
    std::cout << "=== Comparison Tests ===\n";
    
    mystd::tuple<int, std::string> a(1, "test");
    mystd::tuple<int, std::string> b(1, "test");
    mystd::tuple<int, std::string> c(2, "test");
    mystd::tuple<int, std::string> d(1, "test2");
    
    // Equality
    test_utils::assert_true(a == b, "Equality true");
    test_utils::assert_true(a != c, "Inequality true");
    test_utils::assert_true(a != d, "Inequality true different elements");
    
    // Relational
    test_utils::assert_true(a < c, "Less than");
    test_utils::assert_true(a < d, "Less than different elements");
    test_utils::assert_true(c > a, "Greater than");
    test_utils::assert_true(a <= a, "Less than or equal");
    test_utils::assert_true(b >= a, "Greater than or equal");
    
    // Three-way comparison
    mystd::tuple<ThreeWayComparable, int> e(ThreeWayComparable{1}, 10);
    mystd::tuple<ThreeWayComparable, int> f(ThreeWayComparable{2}, 10);
    test_utils::assert_true(e < f, "Three-way comparison less");
    
    std::cout << "\n";
}

void test_utilities() {
    std::cout << "=== Utility Functions Tests ===\n";
    
    // make_tuple
    auto t1 = mystd::make_tuple(42, 3.14, "hello");
    test_utils::assert_equal(mystd::get<0>(t1), 42, "make_tuple int");
    test_utils::assert_equal(mystd::get<1>(t1), 3.14, "make_tuple double");
    test_utils::assert_equal(mystd::get<2>(t1), "hello", "make_tuple string");
    
    // tie
    int x = 1;
    double y = 2.0;
    std::string z = "three";
    auto t2 = mystd::tie(x, y, z);
    test_utils::assert_equal(mystd::get<0>(t2), 1, "tie int");
    test_utils::assert_equal(mystd::get<1>(t2), 2.0, "tie double");
    test_utils::assert_equal(mystd::get<2>(t2), "three", "tie string");
    
    // Modify through tie
    mystd::get<0>(t2) = 100;
    test_utils::assert_equal(x, 100, "modification through tie");
    
    // tuple_cat
    mystd::tuple<int> t4(1);
    mystd::tuple<double, std::string> t5(2.0, "three");
    mystd::tuple<char> t6('d');
    auto t7 = mystd::tuple_cat(t4, t5, t6);
    test_utils::assert_equal(mystd::get<0>(t7), 1, "tuple_cat element 0");
    test_utils::assert_equal(mystd::get<1>(t7), 2.0, "tuple_cat element 1");
    test_utils::assert_equal(mystd::get<2>(t7), "three", "tuple_cat element 2");
    test_utils::assert_equal(mystd::get<3>(t7), 'd', "tuple_cat element 3");
    
    std::cout << "\n";
}

void test_swap() {
    std::cout << "=== Swap Tests ===\n";
    
    // Member swap
    mystd::tuple<int, std::string> a(1, "one");
    mystd::tuple<int, std::string> b(2, "two");
    a.swap(b);
    test_utils::assert_equal(mystd::get<0>(a), 2, "member swap a int");
    test_utils::assert_equal(mystd::get<1>(a), "two", "member swap a string");
    test_utils::assert_equal(mystd::get<0>(b), 1, "member swap b int");
    test_utils::assert_equal(mystd::get<1>(b), "one", "member swap b string");
    
    // Non-member swap
    mystd::tuple<int, std::string> c(10, "ten");
    mystd::tuple<int, std::string> d(20, "twenty");
    mystd::swap(c, d);
    test_utils::assert_equal(mystd::get<0>(c), 20, "non-member swap c int");
    test_utils::assert_equal(mystd::get<1>(c), "twenty", "non-member swap c string");
    test_utils::assert_equal(mystd::get<0>(d), 10, "non-member swap d int");
    test_utils::assert_equal(mystd::get<1>(d), "ten", "non-member swap d string");
    
    std::cout << "\n";
}

void test_special_types() {
    std::cout << "=== Special Types Tests ===\n";
    
    // Non-copyable types
    mystd::tuple<NonCopyable, int> t1{NonCopyable{42}, 100};
    mystd::tuple<NonCopyable, int> t2{std::move(t1)};
    test_utils::assert_equal(mystd::get<1>(t2), 100, "NonCopyable move construction");
    
    // Non-movable types
    mystd::tuple<NonMovable, int> t3{NonMovable{200}, 300};
    mystd::tuple<NonMovable, int> t4{t3}; // copy construction
    test_utils::assert_equal(mystd::get<1>(t4), 300, "NonMovable copy construction");
    
    // References
    int x = 10;
    std::string y = "ref";
    mystd::tuple<int&, std::string&> t5(x, y);
    mystd::get<0>(t5) = 20;
    mystd::get<1>(t5) = "modified";
    test_utils::assert_equal(x, 20, "reference modification int");
    test_utils::assert_equal(y, "modified", "reference modification string");
    
    // Empty tuple
    mystd::tuple<> empty1;
    mystd::tuple<> empty2;
    empty1.swap(empty2); // Should compile and do nothing
    test_utils::assert_true(empty1 == empty2, "empty tuple equality");
    
    std::cout << "\n";
}

void test_pairs() {
    std::cout << "=== Pair Interoperability Tests ===\n";
    
    // Construction from pair
    std::pair<int, std::string> p1(42, "pair");
    mystd::tuple<int, std::string> t1(p1);
    test_utils::assert_equal(mystd::get<0>(t1), 42, "construction from pair int");
    test_utils::assert_equal(mystd::get<1>(t1), "pair", "construction from pair string");
    
    // Construction from moved pair
    std::pair<std::string, int> p2("hello", 42);
    mystd::tuple<std::string, int> t2(std::move(p2));
    test_utils::assert_equal(mystd::get<0>(t2), "hello", "construction from moved pair string");
    test_utils::assert_equal(mystd::get<1>(t2), 42, "construction from moved pair int");
    
    // Assignment from pair
    mystd::tuple<int, std::string> t3(0, "");
    std::pair<int, std::string> p3(100, "assigned");
    t3 = p3;
    test_utils::assert_equal(mystd::get<0>(t3), 100, "assignment from pair int");
    test_utils::assert_equal(mystd::get<1>(t3), "assigned", "assignment from pair string");
    
    std::cout << "\n";
}

void test_type_traits() {
    std::cout << "=== Type Traits Tests ===\n";
    
    using Tuple = mystd::tuple<int, double, std::string>;
    
    // tuple_size
    test_utils::assert_true(std::tuple_size_v<Tuple> == 3, "tuple_size");
    test_utils::assert_true(std::tuple_size_v<const Tuple> == 3, "tuple_size const");
    test_utils::assert_true(std::tuple_size_v<volatile Tuple> == 3, "tuple_size volatile");
    
    std::cout << "\n";
}

void test_edge_cases() {
    std::cout << "=== Edge Cases Tests ===\n";
    
    // Single element tuple
    mystd::tuple<int> t1(42);
    test_utils::assert_equal(mystd::get<0>(t1), 42, "single element tuple");
    
    mystd::tuple<int> t2 = t1;
    test_utils::assert_equal(mystd::get<0>(t2), 42, "single element tuple copy");
    
    // ignore
    int important = 42;
    double also_important = 3.14;
    mystd::tie(important, mystd::ignore, also_important) = mystd::make_tuple(100, "ignored", 6.28);
    test_utils::assert_equal(important, 100, "ignore with tie modification");
    test_utils::assert_equal(also_important, 6.28, "ignore with tie modification second");
    
    // Nested tuples
    mystd::tuple<mystd::tuple<int, int>, std::string> t3(
        mystd::tuple<int, int>(1, 2), "nested"
    );
    test_utils::assert_equal(mystd::get<0>(mystd::get<0>(t3)), 1, "nested tuple element 0");
    test_utils::assert_equal(mystd::get<1>(mystd::get<0>(t3)), 2, "nested tuple element 1");
    test_utils::assert_equal(mystd::get<1>(t3), "nested", "nested tuple main element");
    
    // CV-qualified tuples
    const mystd::tuple<int, std::string> ct(42, "const");
    test_utils::assert_equal(mystd::get<0>(ct), 42, "const tuple access");
    
    std::cout << "\n";
}

void test_ctad() {
    std::cout << "=== CTAD Tests ===\n";
    
    // From values
    mystd::tuple t1(42, 3.14, "hello");
    test_utils::assert_true((std::is_same_v<decltype(t1), mystd::tuple<int, double, const char*>>), "CTAD from values");
    
    // From pair
    std::pair p(1, "pair");
    mystd::tuple t2(p);
    test_utils::assert_true((std::is_same_v<decltype(t2), mystd::tuple<int, const char*>>), "CTAD from pair");
    
    std::cout << "\n";
}

void test_allocator_aware() {
    std::cout << "=== Allocator-Aware Tests ===\n";
    
    std::allocator<int> alloc;
    
    // With allocator_arg
    mystd::tuple<int, std::string> t1(mystd::allocator_arg, alloc, 42, "alloc");
    test_utils::assert_equal(mystd::get<0>(t1), 42, "allocator construction int");
    test_utils::assert_equal(mystd::get<1>(t1), "alloc", "allocator construction string");
    
    // Copy with allocator
    mystd::tuple<int, std::string> orig(100, "original");
    mystd::tuple<int, std::string> t2(mystd::allocator_arg, alloc, orig);
    test_utils::assert_equal(mystd::get<0>(t2), 100, "allocator copy construction int");
    test_utils::assert_equal(mystd::get<1>(t2), "original", "allocator copy construction string");
    
    std::cout << "\n";
}

void test_ignore_functionality() {
    std::cout << "=== Ignore Functionality Tests ===\n";
    
    auto t = mystd::make_tuple(1, 2, 3);
    
    // Normal unpacking
    auto [a, b, c] = t;
    test_utils::assert_equal(a, 1, "normal unpacking 1");
    test_utils::assert_equal(b, 2, "normal unpacking 2");
    test_utils::assert_equal(c, 3, "normal unpacking 3");
    
    // With ignore
    auto [x, mystd::ignore, z] = t;
    test_utils::assert_equal(x, 1, "ignore unpacking first");
    test_utils::assert_equal(z, 3, "ignore unpacking last");
    // The middle element (2) is ignored
    
    std::cout << "\n";
}

void test_compatibility() {
    std::cout << "=== Compatibility Tests ===\n";
    
    // Compile-time compatibility checks
    test_utils::assert_true((std::tuple_size_v<mystd::tuple<int, double, std::string>> == 3), "compatibility tuple_size");
    test_utils::assert_true((std::is_same_v<std::tuple_element_t<0, mystd::tuple<int, double>>, int>), "compatibility tuple_element");
    
    // Ensure structured binding works
    mystd::tuple<int, std::string> t(42, "test");
    auto& [i, s] = t;
    test_utils::assert_equal(i, 42, "compatibility structured binding int");
    test_utils::assert_equal(s, "test", "compatibility structured binding string");
    
    std::cout << "\n";
}

int main() {
    std::cout << "Running mystd::tuple comprehensive tests...\n\n";
    
    test_utils::reset();
    
    test_basic_construction();
    test_element_access();
    test_assignment();
    test_comparison();
    test_utilities();
    test_swap();
    test_special_types();
    test_pairs();
    test_type_traits();
    test_edge_cases();
    test_ctad();
    test_allocator_aware();
    test_ignore_functionality();
    test_compatibility();
    
    std::cout << "=== FINAL RESULTS ===\n";
    test_utils::print_stats();
    
    return test_utils::passed_count == test_utils::test_count ? 0 : 1;
}
