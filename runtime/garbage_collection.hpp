#pragma once

class stack_ref;
extern bool garbage_state;

void dump_memory();
stack_ref request_stack();
void print_allocated();
