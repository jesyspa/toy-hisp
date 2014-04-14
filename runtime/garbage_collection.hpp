#pragma once

class SubStack;

void init_gc();
void dump_memory();
void collect_garbage();

SubStack request_stack();
bool is_heap_ptr(void const* ptr);
