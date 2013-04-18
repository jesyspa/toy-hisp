#pragma once
#include "main.hpp"
#include <cstdio>

struct graph {
    ref r;
};

void internal_print(ref r);
void graphviz_dump(graph g);

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
