#pragma once

#include "hisp/object.hpp"
#include <boost/mpl/vector.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/find.hpp>
#include <type_traits>

namespace detail {
namespace mpl = boost::mpl;
namespace ph = mpl::placeholders;

template <typename T>
struct ObjectTypeTag;

using ObjectTypes = mpl::vector<Application, Number, Function, Forwarder>;

template <typename T>
constexpr ObjectType get_type() noexcept {
    using iter = typename mpl::find<ObjectTypes, T>::type;
    using end = mpl::end<ObjectTypes>::type;
    static_assert(!std::is_same<iter, end>::value, "type not found");
    using pos = typename iter::pos;
    return pos::value;
}
}

using detail::ObjectTypes;
using detail::get_type;
