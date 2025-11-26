// test_tuple.cpp
#include "tuple.hpp"
#include <cassert>
#include <string>
#include <memory>
#include <type_traits>
#include <iostream>

// Test struct with custom constructor
struct TestStruct {
    int x;
    std::string s;
    
    TestStruct() : x(0), s("default") {}
    TestStruct(int x, const std::string& s) : x(x), s(s) {}
    
    bool operator==(const TestStruct& other) const {
        return x == other.x && s == other.s;
    }
};

// Test allocator
template<typename T>
struct TestAllocator {
    using value_type = T;
    
    TestAllocator() = default;
    
    template<typename U>
    TestAllocator(const TestAllocator<U>&) {}
    
    T* allocate(std::size_t n) {
        return static_cast<T*>(::operator new(n * sizeof(T)));
    }
    
    void deallocate(T* p, std::size_t) {
        ::operator delete(p);
    }
    
    template<typename U>
    bool operator==(const TestAllocator<U>&) const { return true; }
    
    template<typename U>
    bool operator!=(const TestAllocator<U>&) const { return false; }
};

void test_basic_construction() {
    std::cout << "Testing basic construction...\n";
    
    // Default construction
    mystd::tuple<int, double, std::string> t1;
    assert(mystd::get<0>(t1) == 0);
    assert(mystd::get<1>(t1) == 0.0);
    assert(mystd::get<2>(t1) == "");
    
    // Value construction
    mystd::tuple<int, double, std::string> t2(42, 3.14, "hello");
    assert(mystd::get<0>(t2) == 42);
    assert(mystd::get<1>(t2) == 3.14);
    assert(mystd::get<2>(t2) == "hello");
    
    // Copy construction
    mystd::tuple<int, double, std::string> t3(t2);
    assert(mystd::get<0>(t3) == 42);
    assert(mystd::get<1>(t3) == 3.14);
    assert(mystd::get<2>(t3) == "hello");
    
    // Move construction
    mystd::tuple<int, double, std::string> t4(std::move(t2));
    assert(mystd::get<0>(t4) == 42);
    assert(mystd::get<1>(t4) == 3.14);
    assert(mystd::get<2>(t4) == "hello");
    
    std::cout << "Basic construction tests passed!\n";
}

void test_assignment() {
    std::cout << "Testing assignment...\n";
    
    mystd::tuple<int, double, std::string> t1(1, 2.0, "a");
    mystd::tuple<int, double, std::string> t2(3, 4.0, "b");
    
    // Copy assignment
    t1 = t2;
    assert(mystd::get<0>(t1) == 3);
    assert(mystd::get<1>(t1) == 4.0);
    assert(mystd::get<2>(t1) == "b");
    
    // Move assignment
    mystd::tuple<int, double, std::string> t3(5, 6.0, "c");
    t1 = std::move(t3);
    assert(mystd::get<0>(t1) == 5);
    assert(mystd::get<1>(t1) == 6.0);
    assert(mystd::get<2>(t1) == "c");
    
    std::cout << "Assignment tests passed!\n";
}

void test_get() {
    std::cout << "Testing get...\n";
    
    mystd::tuple<int, double, std::string> t(42, 3.14, "hello");
    
    // get by index
    assert(mystd::get<0>(t) == 42);
    assert(mystd::get<1>(t) == 3.14);
    assert(mystd::get<2>(t) == "hello");
    
    // get by type
    assert(mystd::get<int>(t) == 42);
    assert(mystd::get<double>(t) == 3.14);
    assert(mystd::get<std::string>(t) == "hello");
    
    // const get
    const auto& ct = t;
    assert(mystd::get<0>(ct) == 42);
    assert(mystd::get<int>(ct) == 42);
    
    std::cout << "Get tests passed!\n";
}

void test_structured_binding() {
    std::cout << "Testing structured binding support...\n";
    
    mystd::tuple<int, double, std::string> t(42, 3.14, "hello");
    
    auto& [a, b, c] = t;
    assert(a == 42);
    assert(b == 3.14);
    assert(c == "hello");
    
    a = 100;
    assert(mystd::get<0>(t) == 100);
    
    std::cout << "Structured binding tests passed!\n";
}

void test_comparison() {
    std::cout << "Testing comparison operators...\n";
    
    mystd::tuple<int, double, std::string> t1(1, 2.0, "a");
    mystd::tuple<int, double, std::string> t2(1, 2.0, "a");
    mystd::tuple<int, double, std::string> t3(2, 2.0, "a");
    mystd::tuple<int, double, std::string> t4(1, 3.0, "a");
    
    // Equality
    assert(t1 == t2);
    assert(!(t1 == t3));
    
    // Inequality
    assert(t1 != t3);
    assert(!(t1 != t2));
    
    // Less than
    assert(t1 < t3);
    assert(!(t3 < t1));
    
    // Spaceship
    assert((t1 <=> t2) == std::strong_ordering::equal);
    assert((t1 <=> t3) == std::strong_ordering::less);
    
    std::cout << "Comparison tests passed!\n";
}

void test_swap() {
    std::cout << "Testing swap...\n";
    
    mystd::tuple<int, std::string> t1(1, "a");
    mystd::tuple<int, std::string> t2(2, "b");
    
    t1.swap(t2);
    
    assert(mystd::get<0>(t1) == 2);
    assert(mystd::get<1>(t1) == "b");
    assert(mystd::get<0>(t2) == 1);
    assert(mystd::get<1>(t2) == "a");
    
    mystd::swap(t1, t2);
    
    assert(mystd::get<0>(t1) == 1);
    assert(mystd::get<1>(t1) == "a");
    assert(mystd::get<0>(t2) == 2);
    assert(mystd::get<1>(t2) == "b");
    
    std::cout << "Swap tests passed!\n";
}

void test_allocator_construction() {
    std::cout << "Testing allocator construction...\n";
    
    TestAllocator<int> alloc;
    
    // Default construction with allocator
    mystd::tuple<int, double, std::string> t1(mystd::allocator_arg, alloc);
    assert(mystd::get<0>(t1) == 0);
    assert(mystd::get<1>(t1) == 0.0);
    assert(mystd::get<2>(t1) == "");
    
    // Value construction with allocator
    mystd::tuple<int, double, std::string> t2(mystd::allocator_arg, alloc, 42, 3.14, "hello");
    assert(mystd::get<0>(t2) == 42);
    assert(mystd::get<1>(t2) == 3.14);
    assert(mystd::get<2>(t2) == "hello");
    
    // Copy construction with allocator
    mystd::tuple<int, double, std::string> t3(mystd::allocator_arg, alloc, t2);
    assert(mystd::get<0>(t3) == 42);
    assert(mystd::get<1>(t3) == 3.14);
    assert(mystd::get<2>(t3) == "hello");
    
    // Move construction with allocator
    mystd::tuple<int, double, std::string> t4(mystd::allocator_arg, alloc, std::move(t2));
    assert(mystd::get<0>(t4) == 42);
    assert(mystd::get<1>(t4) == 3.14);
    assert(mystd::get<2>(t4) == "hello");
    
    std::cout << "Allocator construction tests passed!\n";
}

void test_utilities() {
    std::cout << "Testing utility functions...\n";
    
    // make_tuple
    auto t1 = mystd::make_tuple(42, 3.14, "hello");
    static_assert(std::is_same_v<decltype(t1), mystd::tuple<int, double, const char*>>);
    assert(mystd::get<0>(t1) == 42);
    
    // tie
    int a = 1;
    double b = 2.0;
    std::string c = "test";
    auto t2 = mystd::tie(a, b, c);
    mystd::get<0>(t2) = 10;
    assert(a == 10);
    
    // forward_as_tuple
    auto t3 = mystd::forward_as_tuple(42, 3.14, std::move(c));
    static_assert(std::is_same_v<decltype(mystd::get<2>(t3)), std::string&&>);
    
    // tuple_cat
    auto t4 = mystd::make_tuple(1, "a");
    auto t5 = mystd::make_tuple(2.0, 'b');
    auto t6 = mystd::tuple_cat(t4, t5);
    assert(mystd::get<0>(t6) == 1);
    assert(mystd::get<1>(t6) == "a");
    assert(mystd::get<2>(t6) == 2.0);
    assert(mystd::get<3>(t6) == 'b');
    
    std::cout << "Utility function tests passed!\n";
}

void test_special_cases() {
    std::cout << "Testing special cases...\n";
    
    // Empty tuple
    mystd::tuple<> empty;
    
    // Single element tuple
    mystd::tuple<int> single(42);
    assert(mystd::get<0>(single) == 42);
    
    // Pair conversion
    std::pair<int, std::string> p(1, "pair");
    mystd::tuple<int, std::string> t(p);
    assert(mystd::get<0>(t) == 1);
    assert(mystd::get<1>(t) == "pair");
    
    // Move from pair
    mystd::tuple<int, std::string> t2(std::move(p));
    
    std::cout << "Special case tests passed!\n";
}

void test_custom_types() {
    std::cout << "Testing custom types...\n";
    
    TestStruct ts1(42, "test");
    mystd::tuple<TestStruct, int> t1(ts1, 100);
    assert(mystd::get<0>(t1) == ts1);
    assert(mystd::get<1>(t1) == 100);
    
    // Move custom type
    mystd::tuple<TestStruct> t2(TestStruct(10, "move"));
    assert(mystd::get<0>(t2).x == 10);
    
    std::cout << "Custom type tests passed!\n";
}

void test_tuple_size_and_element() {
    std::cout << "Testing tuple_size and tuple_element...\n";
    
    using Tuple = mystd::tuple<int, double, std::string>;
    
    static_assert(mystd::tuple_size_v<Tuple> == 3);
    static_assert(std::is_same_v<mystd::tuple_element_t<0, Tuple>, int>);
    static_assert(std::is_same_v<mystd::tuple_element_t<1, Tuple>, double>);
    static_assert(std::is_same_v<mystd::tuple_element_t<2, Tuple>, std::string>);
    
    std::cout << "Tuple size and element tests passed!\n";
}

int main() {
    try {
        test_basic_construction();
        test_assignment();
        test_get();
        test_structured_binding();
        test_comparison();
        test_swap();
        test_allocator_construction();
        test_utilities();
        test_special_cases();
        test_custom_types();
        test_tuple_size_and_element();
        
        std::cout << "\nAll tests passed successfully!\n";
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "Test failed with exception: " << e.what() << "\n";
        return 1;
    }
}