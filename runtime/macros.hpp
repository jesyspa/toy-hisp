#pragma once
/* \file
 * \brief Collection of helper macros.
 */

#include "garbage_collection.hpp"
#include <cassert>

#ifndef NDEBUG
//! \brief Assert that the object and its pointers all have reasonable values.
#define ASSERT_SANITY(r)                                                                                               \
    do {                                                                                                               \
        assert(r&& is_heap_ptr(r));                                                                                    \
        if (auto app = try_cast<Application>(r)) {                                                                     \
            assert(app->left&& is_heap_ptr(app->left));                                                                \
            assert(app->right&& is_heap_ptr(app->right));                                                              \
        } else if (auto fwd = try_cast<Forwarder>(r)) {                                                                \
            assert(fwd->target&& is_heap_ptr(fwd->target));                                                            \
        }                                                                                                              \
    } while (false)
#else
#define ASSERT_SANITY(r)                                                                                               \
    do {                                                                                                               \
        (void) r;                                                                                                      \
    } while (false)
#endif
