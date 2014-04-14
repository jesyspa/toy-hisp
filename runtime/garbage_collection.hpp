#pragma once

class stack_ref;

extern bool garbage_state;

void init_gc();
void dump_memory();
stack_ref request_stack();
bool is_heap_ptr(void const* ptr);
