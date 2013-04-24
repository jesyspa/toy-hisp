#pragma once
#include "main.hpp"
#include <cstdio>

struct graph {
    ref r;
};

struct multi_graph {
    ref r;
};

void internal_print(ref r);
void graphviz_dump(graph g);
void multi_graphviz_dump(multi_graph g);

inline void print_one(int i) {
    printf("%d", i);
}

inline void print_one(char const* p) {
    printf("%s", p);
}

inline void print_one(ref r) {
    internal_print(r);
}

inline void print_one(graph g) {
    graphviz_dump(g);
}

inline void print_one(multi_graph g) {
    multi_graphviz_dump(g);
}

inline void print_one(void* p) {
    printf("%p", p);
}

template<typename...>
struct debug_print_impl;

template<typename T, typename... ARGS>
struct debug_print_impl<T, ARGS...> {
    static void invoke(T t, ARGS... args) {
#ifndef NDEBUG
        print_one(t);
#else
        (void)t;
#endif
        debug_print_impl<ARGS...>::invoke(args...);
    }
};

template<>
struct debug_print_impl<> {
    static void invoke() {}
};

template<typename... ARGS>
void debug_print(ARGS... args) {
    debug_print_impl<ARGS...>::invoke(args...);
    getchar();
}
