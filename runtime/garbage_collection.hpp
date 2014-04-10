#pragma once

#include "main.hpp"

#define CONCAT_IMPL(a, b) a##b
#define CONCAT(a, b) CONCAT_IMPL(a, b)
#define PRESERVE(x) root CONCAT(guard_, __COUNTER__ ) (x)

extern bool garbage_state;
std::size_t const STACK_COUNT = 16;

stack& request_stack();
void release_stack(stack&);
void print_allocated();
