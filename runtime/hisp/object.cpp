#include "hisp/object.hpp"
#include "hisp/utility.hpp"
#include "meta/type_index.hpp"

void rewrite_as_forwarder(Ref obj, Ref target) {
    assert(obj != target && "forwarding to yourself does *not* work!");
    obj->type = get_type<Forwarder>();
    reinterpret_cast<Forwarder*>(obj)->target = target;
}
