#pragma once

#include "main.hpp"

struct stack_link {
    stack prev;
    application* arg;
};

void push(stack& sl, application* app);

WARN_UNUSED_RESULT
application* extract(stack& sl);

void pop(stack& sl);
