#pragma once

class SubStack;

// Initialize the garbage collector.  No allocations should be done until this is called.
void init_gc();

// Perform a garbage collection.  Invalidates all pointers into the heap.
void collect_garbage();

// Get an (empty) stack starting at the top of the current program stack.
SubStack request_stack();

// Check whether a pointer points into the (current) heap.
//
// Mostly for sanity checking.
bool is_heap_ptr(void const* ptr);
