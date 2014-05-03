#pragma once

#include "debug/stream_dispenser.hpp"
#include "hisp/object.hpp"
#include "memory/stack.hpp"
#include "memory/space.hpp"

//! \brief Singleton for improving debug information.
class Debugger {
    struct DebuggerImpl {
        StreamDispenser graph_streams = StreamDispenser("graph", "dot");
        StreamDispenser array_streams = StreamDispenser("memory", "dump");

        void dump_graph_beneath(CRef r);
        void dump_memory_as_graph();
        void dump_memory_as_array();
        void print_expression(CRef r);
        void step();
    };

    //! \brief Get a pointer to the instance of the implementation.
    //
    //  Returns nullptr if debugging is disabled.
    static DebuggerImpl* get_instance() {
#ifndef NDEBUG
        static DebuggerImpl impl;
        return &impl;
#else
        return nullptr;
#endif
    }

public:
    //! \brief Dump the part of the object graph reachable from root.
    static void dump_graph_beneath(CRef root) {
        if (auto impl = get_instance())
            impl->dump_graph_beneath(root);
    }

    //! \brief Dump the whole memory graph in dot format.
    static void dump_memory_as_graph() {
        if (auto impl = get_instance())
            impl->dump_memory_as_graph();
    }

    /*! \brief Dump all of allocated memory as-is, in hexadecimal.
     *
     *  Also dumps the stack.
     */
    static void dump_memory_as_array() {
        if (auto impl = get_instance())
            impl->dump_memory_as_array();
    }

    //! \brief Print a human-readable version of the expression starting at root.
    static void print_expression(CRef root) {
        if (auto impl = get_instance())
            impl->print_expression(root);
    }

    /*! \brief Call this to indicate that a step has been taken.
     *
     *  Automatically decides what must be done based on the current configuration.
     */
    static void step() {
        if (auto impl = get_instance())
            impl->step();
    }
};

/*! \brief Full information about the object graph and root set.
 *
 *  Do \em not use this for anything but displaying debugging information.
 */
struct DebugMemoryInfo {
    StackStorage const* stack;
    Space const* space;
};
