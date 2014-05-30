#pragma once

#include "hisp/object_traits.hpp"
#include "meta/type_index.hpp"
#include <boost/mpl/reverse_fold.hpp>
#include <boost/mpl/lambda.hpp>
#include <boost/mpl/quote.hpp>
#include <stdexcept>

namespace detail {
namespace mpl = boost::mpl;
namespace ph = mpl::placeholders;

template <typename F, typename LHS, typename RHS>
struct RAOp {
    template <typename... Args>
    static void execute(Ref obj, Args&&... args) {
        if (obj->type == get_type<LHS>())
            F::execute(static_cast<LHS*>(obj), std::forward<Args>(args)...);
        else
            RHS::execute(obj, std::forward<Args>(args)...);
    }
};

struct RAState {
    template <typename... Args>
    static void execute(Ref, Args&&...) {
        throw std::runtime_error{"object has invalid type"};
    }
};

template <typename F, typename... Args>
void RuntimeApply(Ref obj, Args&&... args) {
    using Op = RAOp<F, ph::_2, ph::_1>;
    using RA = typename mpl::reverse_fold<ObjectTypes, RAState, Op>::type;
    RA::execute(obj, std::forward<Args>(args)...);
}
}

using detail::RuntimeApply;
