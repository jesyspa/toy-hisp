#pragma once

//#include "main.hpp"

#define CONCAT_IMPL(a, b) a##b
#define CONCAT(a, b) CONCAT_IMPL(a, b)
#define PRESERVE(x) root CONCAT(guard_, __COUNTER__ ) (x)

