#pragma once

#include "main.hpp"

bool empty(stack const& sl);
ref top(stack const& sl);
void push(stack& sl, ref r);
WARN_UNUSED_RESULT
ref extract(stack& sl);
void pop(stack& sl);
