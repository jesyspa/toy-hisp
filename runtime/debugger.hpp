#pragma once

#include "stream_dispenser.hpp"
#include "object.hpp"
#include "stack.hpp"
#include "space.hpp"

// Singleton class to dump intermediate results
class Debugger {
    struct DebuggerImpl {
        StreamDispenser for_graphs = StreamDispenser("graph", "dot");
        StreamDispenser for_arrays = StreamDispenser("memory", "dump");

        void dump_graph_beneath(CRef r);
        void dump_memory_as_graph();
        void dump_memory_as_array();
        void print_expression(CRef r);
        void step();
    };

    static DebuggerImpl* get_instance() {
#ifndef NDEBUG
        static DebuggerImpl impl;
        return &impl;
#else
        return nullptr;
#endif
    }

public:
    static void dump_graph_beneath(CRef r) {
        if (auto impl = get_instance())
            impl->dump_graph_beneath(r);
    }

    static void dump_memory_as_graph() {
        if (auto impl = get_instance())
            impl->dump_memory_as_graph();
    }

    static void dump_memory_as_array() {
        if (auto impl = get_instance())
            impl->dump_memory_as_array();
    }

    static void print_expression(CRef r) {
        if (auto impl = get_instance())
            impl->print_expression(r);
    }

    // Call this to indicate that a logical step has been taken in the execution of the program.
    // TODO: Allow specifying the size of the step.
    static void step() {
        if (auto impl = get_instance())
            impl->step();
    }
};

struct DebugMemoryInfo {
    Stack const* stack;
    Space const* space;
};
