#pragma once

#include "hisp/object_traits.hpp"
#include <type_traits>
#include <boost/mpl/fold.hpp>

namespace detail {
struct RMAState {
    template <typename... Args>
    static void execute(Args&&...) {}
};

template <typename Op, typename LHS, typename RHS>
struct RMAOp {
    template <typename... Args>
    static void execute(Args&&... args) {
        LHS::execute(std::forward<Args>(args)...);
        Op::template execute<RHS>(typename RHS::tag{}, std::forward<Args>(args)...);
    }
};
}

template <typename Op, typename Obj, typename... Args>
void RuntimeMemberwiseApply(Args&&... args) {
    using namespace detail;
    using namespace boost::mpl::placeholders;
    using members = typename ObjectTraits<Obj>::members;
    using RMA = typename boost::mpl::fold<members, RMAState, RMAOp<Op, _1, _2>>::type;
    RMA::execute(std::forward<Args>(args)...);
}
