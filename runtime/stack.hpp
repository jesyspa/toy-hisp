#pragma once

#include "main.hpp"

void push(stack& sl, application* app);

WARN_UNUSED_RESULT
safe_ref<application> extract(stack& sl);

void pop(stack& sl);
