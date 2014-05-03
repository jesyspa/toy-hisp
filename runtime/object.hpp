#pragma once

#include <cstddef>
#include <cstdint>

//! \brief Type of a Hisp object.
enum class ObjectType { application_object, number_object, function_object, forwarder_object };

/*! \brief A Hisp object.
 *
 *  This class contains the header information that all objects need.  Concrete object types should derive from this and
 *  provide a static constexpr ObjectType TYPE member to indicate their real type.
 */
struct Object {
    //! \brief The object's type.
    ObjectType type;

    /*! \brief The size of the allocated object in bytes.
     *
     *  This is the number of bytes actually allocated; the object may require less.
     */
    std::uint32_t size;

    //! \brief Pointer to the latest copy of this object.
    Object* forward;

    Object(Object const&) = delete;
    Object& operator=(Object const&) = delete;
};

using Ref = Object*;
using CRef = Object const*;

class Stack;
using Func = void (*)(Stack);

//! \brief Hisp representation of a function application.
struct Application : Object {
    //! \brief The function to be applied.
    Ref left;

    //! \brief The argument of the function.
    Ref right;

    static constexpr ObjectType TYPE = ObjectType::application_object;
};

//! \brief Hisp representation of a number.
struct Number : Object {
    //! \brief Value of the number.
    int value;
    static constexpr ObjectType TYPE = ObjectType::number_object;
};

//! \brief Hisp representation of a (built-in) function.
struct Function : Object {
    //! \brief Pointer to the compiled implementation of this function.
    Func func;
    static constexpr ObjectType TYPE = ObjectType::function_object;
};

//! \brief Meta-object that indicates that the result of this node is elsewhere.
struct Forwarder : Object {
    Ref target;
    static constexpr ObjectType TYPE = ObjectType::forwarder_object;
};

//! \brief Destroy obj, turning it into an object that forwards to target.
void rewrite_as_forwarder(Ref obj, Ref target);
