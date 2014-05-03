#include "debug/stream_ptr.hpp"
#include <fstream>

OStreamPtr open_file(std::string filename) {
    auto file_ptr = new std::ofstream(std::move(filename));
    auto deleter = [](std::ostream* ptr) { delete ptr; };
    return {file_ptr, deleter};
}

// TODO: Come up with a better name for this.
OStreamPtr take_pointer(std::ostream& os) {
    return {&os, [](std::ostream*) {}};
}
