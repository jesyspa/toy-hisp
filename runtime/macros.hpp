#pragma once

#include "garbage_collection.hpp"

#ifdef __GNUC__
#define WARN_UNUSED_RESULT __attribute__((warn_unused_result))
#else
#define WARN_UNUSED_RESULT
#endif

#ifndef NDEBUG
#define ASSERT_SANITY(r)                                                                                               \
    do {                                                                                                               \
        assert(r&& is_heap_ptr(r));                                                                                    \
        if (auto app = try_cast<Application>(r)) {                                                                     \
            assert(app->left&& is_heap_ptr(app->left));                                                                \
            assert(app->right&& is_heap_ptr(app->right));                                                              \
        }                                                                                                              \
    } while (false)
#else
#define ASSERT_SANITY(r)                                                                                               \
    do {                                                                                                               \
        (void) r;                                                                                                      \
    } while (false)
#endif
