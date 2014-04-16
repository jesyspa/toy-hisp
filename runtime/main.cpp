#include "eval.hpp"
#include "garbage_collection.hpp"
#include "stack.hpp"

int main() {
    auto s = use_init_file();
    eval(s);
    collect_garbage();
    deinit_gc();
}
