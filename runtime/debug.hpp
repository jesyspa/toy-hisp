#pragma once
#include "main.hpp"
#include <iostream>

struct GraphBag {
    Ref root;
};

struct MultiGraphBag {
    Stack const& stack;
    char* space;
    std::size_t size;
};

struct MemoryBag {
    Stack const& stack;
    char* space;
    std::size_t size;
};

void print_expression(Ref root);
void graphviz_dump(GraphBag graph);
void multi_graphviz_dump(MultiGraphBag graph);
void raw_dump(MemoryBag memory);

inline void print_one(int i) {
    std::cerr << i;
}

inline void print_one(char const* p) {
    std::cerr << p;
}

inline void print_one(Ref root) {
    print_expression(root);
}

inline void print_one(GraphBag graph) {
    graphviz_dump(graph);
}

inline void print_one(MultiGraphBag graph) {
    multi_graphviz_dump(graph);
}

inline void print_one(void* p) {
    std::cerr << p;
}

inline void print_one(MemoryBag memory) {
    raw_dump(memory);
}

template<typename...>
struct DebugPrintImpl;

template<typename T, typename... ARGS>
struct DebugPrintImpl<T, ARGS...> {
    static void invoke(T t, ARGS... args) {
#ifndef NDEBUG
        print_one(t);
#else
        (void)t;
#endif
        DebugPrintImpl<ARGS...>::invoke(args...);
    }
};

template<>
struct DebugPrintImpl<> {
    static void invoke() {}
};

template<typename... ARGS>
void debug_print(ARGS... args) {
    DebugPrintImpl<ARGS...>::invoke(args...);
    std::cin.get();
}
