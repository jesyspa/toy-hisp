#pragma once

#include "hisp/object.hpp"
#include <string>

class Stack;
struct DebugMemoryInfo;

//! \brief Perform a garbage collection.  Invalidates all pointers into the heap.
void collect_garbage();

/*! \brief Create an initialization file based on the current program state.
 *
 *  Assumes the graph is not currently being evaluated.
 *
 *  \deprecated This was intended to create the first few init files to test the loading code.  It doesn't belong here
 *  and should be moved out somewhere.
 */
void create_init_file();

/*! \brief Load the given file to be the currently running program.
 *
 * Return a stack with the expression to be evaluated.
 */
Stack use_init_file(std::string name);

//! \brief Get an empty stack at the top of the current program stack.
Stack request_stack() noexcept;

//! \brief Check whether the given object is on the (active) heap.
bool is_heap_ptr(CRef obj) noexcept;

/*! \brief Destroy all objects involved in garbage collection.
 *
 *  Implicitly terminates the currently-executed program.
 */
void deinit_gc() noexcept;

/*! \brief Allocate an application object.
 *
 *  Use the top two values on the stack as left and right respectively.
 */
void make_application(Stack stack);

//! \brief Allocate a number object.
void make_number(Stack stack, int value);

//! \brief Allocate a function object.
void make_function(Stack stack, Func func);

//! \brief Get a full overview of the current object graph and root set.
DebugMemoryInfo get_debug_memory_info();
