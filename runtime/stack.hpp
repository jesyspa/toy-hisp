#pragma once

#include "main.hpp"

bool empty(stack const& sl);
ref top(stack const& sl);
ref get_n(stack const& sl, std::size_t n);
void push(stack& sl, ref r);
WARN_UNUSED_RESULT
ref extract(stack& sl);
void pop(stack& sl);
void pop_n(stack& sl, std::size_t n);

