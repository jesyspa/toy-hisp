#pragma once

#include "object.hpp"

class SubStack;

// Perform a garbage collection.  Invalidates all pointers into the heap.
void collect_garbage();

// Create an initialization file based on the current program state.
// Should not be called while in eval.
void create_init_file();
SubStack use_init_file();

// Get an (empty) stack starting at the top of the current program stack.
SubStack request_stack();

// Check whether a pointer points into the (current) heap.
//
// Mostly for sanity checking.
bool is_heap_ptr(CRef obj);

// Free resources used for garbage collection
void deinit_gc();

// Allocation functions.  All return a pointer to the new object on the stack.
void make_application(SubStack stack);
void make_number(SubStack stack, int value);
void make_function(SubStack stack, Func func);
void make_bool(SubStack stack, bool value);

