#pragma once
#include "object.hpp"
#include "space.hpp"
#include "stack.hpp"
#include <iostream>

// Helper structs for disambiguating what exactly we want to print.

struct GraphBag {
    Ref root;
};

struct MultiGraphBag {
    Stack const& stack;
    Space const& space;
};

struct MemoryBag {
    Stack const& stack;
    Space const& space;
};

// Helper functions to make debug_print print the correct object.
inline void print_one(int i) { std::cerr << i; }

inline void print_one(char const* p) { std::cerr << p; }

inline void print_one(void* p) { std::cerr << p; }

// Print the expression starting at the given root.
inline void print_one(Ref root) {
    void print_expression(Ref root);
    print_expression(root);
}

// Print everything reachable from the given root as a graph.
inline void print_one(GraphBag graph) {
    void graphviz_dump(GraphBag graph);
    graphviz_dump(graph);
}

// Print everything currently allocated, annotated with stack pointers.
inline void print_one(MultiGraphBag graph) {
    void multi_graphviz_dump(MultiGraphBag graph);
    multi_graphviz_dump(graph);
}

// Print all currently allocated memory in a kinda-human-readable format.
inline void print_one(MemoryBag memory) {
    void raw_dump(MemoryBag memory);
    raw_dump(memory);
}

// Print all arguments separated by spaces and followed by a newline, then wait for user input.
template <typename... ARGS>
void debug_print(ARGS... args) {
    auto dummy = {(print_one(args), std::cerr << ' ', 0)...};
    std::cerr << '\n';
    (void)dummy;
    std::cin.get();
}
