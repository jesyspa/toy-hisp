#pragma once

#include "meta/type_index.hpp"
#include <boost/mpl/vector.hpp>

namespace detail {
namespace mpl = boost::mpl;
namespace ph = mpl::placeholders;

template <int... Ints>
struct ISeq {};

template <int B, int T, typename Seq>
struct IntegerSequence;

template <int B, int T, int... Ints>
struct IntegerSequence<B, T, ISeq<Ints...>> {
    using type = typename IntegerSequence<B + 1, T, ISeq<Ints..., B>>::type;
};

template <int T, int... Ints>
struct IntegerSequence<T, T, ISeq<Ints...>> {
    using type = ISeq<Ints...>;
};

template <int N>
using GenSeq = typename IntegerSequence<0, N, ISeq<>>::type;

template <typename T, template <typename> class F>
struct PrepareValue {
    template <int N, typename Arg>
    static T get(Arg) {
        using type = F<typename mpl::at<ObjectTypes, mpl::int_<N>>::type>;
        return type::value;
    }
};

using ObjectTypeCount = typename mpl::size<ObjectTypes>::type;

template <typename T>
using ArrayRef = T const (&)[ObjectTypeCount::value];

template <typename T, template <typename> class F, int... Ints>
ArrayRef<T> make_array_impl(ISeq<Ints...> ints) noexcept {
    static T const array[] = {PrepareValue<T, F>::template get<Ints>(ints)...};
    return array;
}

template <template <typename> class F, typename T = typename F<Object>::value_type>
ArrayRef<T> make_array() noexcept {
    using seq = GenSeq<ObjectTypeCount::value>;
    return make_array_impl<T, F>(seq{});
}
}

using detail::make_array;
