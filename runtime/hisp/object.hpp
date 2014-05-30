#pragma once

#include <cstddef>
#include <cstdint>

//! \brief Type of a Hisp object.
using ObjectType = std::uint32_t;

/*! \brief A Hisp object.
 *
 *  This class contains the header information that all objects need.  Concrete object types should derive from this and
 *  be registered in meta/type_index.hpp.
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
};

//! \brief Hisp representation of a number.
struct Number : Object {
    //! \brief Value of the number.
    std::int64_t value;
};

//! \brief Hisp representation of a (built-in) function.
struct Function : Object {
    //! \brief Pointer to the compiled implementation of this function.
    Func func;
};

//! \brief Meta-object that indicates that the result of this node is elsewhere.
struct Forwarder : Object {
    Ref target;
};

//! \brief Destroy obj, turning it into an object that forwards to target.
void rewrite_as_forwarder(Ref obj, Ref target);
