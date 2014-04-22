#include "object.hpp"
#include "utility.hpp"

void rewrite_as_forwarder(Ref obj, Ref target) {
    assert(obj != target && "forwarding to yourself does *not* work!");
    obj->type = Forwarder::TYPE;
    reinterpret_cast<Forwarder*>(obj)->target = target;
}
