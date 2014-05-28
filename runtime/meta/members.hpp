#pragma once

#include "hisp/object_traits.hpp"
#include <type_traits>
#include <boost/mpl/fold.hpp>

namespace detail {
struct RSAState {
    template <typename... Args>
    static void execute(Args&&...) {}
};

template <typename Op, typename LHS, typename RHS>
struct RSAOp {
    template <typename... Args>
    static void execute(Args&&... args) {
        LHS::execute(std::forward<Args>(args)...);
        Op::template execute<RHS>(typename RHS::tag{}, std::forward<Args>(args)...);
    }
};

template <typename Seq, typename Op, typename... Args>
void RuntimeSeqApply(Args&&... args) {
    namespace ph = boost::mpl::placeholders;
    using RSA = typename boost::mpl::fold<Seq, RSAState, RSAOp<Op, ph::_1, ph::_2>>::type;
    RSA::execute(std::forward<Args>(args)...);
}
}

template <typename Op, typename Obj, typename... Args>
void RuntimeMemberwiseApply(Args&&... args) {
    using members = ObjectMembers<Obj>;
    detail::RuntimeSeqApply<members, Op>(std::forward<Args>(args)...);
}

template <typename Op, typename Obj, typename... Args>
void RuntimeRecMemberwiseApply(Args&&... args) {
    using members = RecObjectMembers<Obj>;
    detail::RuntimeSeqApply<members, Op>(std::forward<Args>(args)...);
}
