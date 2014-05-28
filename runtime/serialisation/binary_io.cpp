#include "serialisation/binary_io.hpp"
#include <iostream>

namespace {
template<typename T>
char* as_char_ptr(T* ptr) {
    return reinterpret_cast<char*>(ptr);
}
}

void write_uint32(std::ostream& os, std::int32_t x) {
    os.write(as_char_ptr(&x), sizeof x);
}

void write_uint64(std::ostream& os, std::int64_t x) {
    os.write(as_char_ptr(&x), sizeof x);
}

std::uint32_t read_uint32(std::istream& is) {
    std::uint32_t x = 0;
    is.read(as_char_ptr(&x), sizeof x);
    return x;
}

std::uint64_t read_uint64(std::istream& is) {
    std::uint64_t x = 0;
    is.read(as_char_ptr(&x), sizeof x);
    return x;
}
