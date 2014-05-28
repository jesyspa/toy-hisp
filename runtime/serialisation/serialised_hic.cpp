#include "serialisation/serialised_hic.hpp"
#include "serialisation/binary_io.hpp"
#include <algorithm>
#include <cstring>
#include <iostream>
#include <stdexcept>

char const* const SerialisedHic::tag = "HISP";

SerialisedHic::SerialisedHic()
    : root_offset{}
    , data{}
    , object_offsets{}
    , rw_index{}
    , object_index{} {}

void SerialisedHic::write_bytes(char const* ptr, std::size_t bytes) {
    if (rw_index + bytes > data.size())
        data.resize(rw_index + bytes);
    std::copy_n(ptr, bytes, data.begin() + rw_index);
    rw_index += bytes;
}

void SerialisedHic::read_bytes(char* ptr, std::size_t bytes) {
    if (rw_index + bytes > data.size())
        throw std::runtime_error{"reading past end of data"};
    std::copy_n(data.begin() + rw_index, bytes, ptr);
    rw_index += bytes;
}

void SerialisedHic::ignore(std::size_t bytes) {
    if (rw_index + bytes > data.size())
        throw std::runtime_error{"reading past end of data"};
    rw_index += bytes;
}

void SerialisedHic::advance(std::size_t bytes) {
    if (rw_index + bytes > data.size())
        data.resize(rw_index + bytes);
    rw_index += bytes;
}

void SerialisedHic::seek_begin() {
    rw_index = 0;
}

void SerialisedHic::start_object() { object_index = rw_index; }

void SerialisedHic::end_object(Ref ptr) {
    object_offsets.insert(offset_map::relation(ptr, object_index));
    object_index = 0;
}

std::size_t SerialisedHic::size() const { return data.size(); }
bool SerialisedHic::at_top() const { return rw_index == data.size(); }

Ref SerialisedHic::from_offset(std::size_t offset) const { return object_offsets.right.at(offset); }
std::size_t SerialisedHic::to_offset(Ref obj) const { return object_offsets.left.at(obj); }

void SerialisedHic::set_root(Ref obj) { root_offset = to_offset(obj);}
Ref SerialisedHic::get_root() const { return from_offset(root_offset); }

void SerialisedHic::write_to_stream(std::ostream& os) const {
    os.write(tag, 4);
    write_uint32(os, 0);
    write_uint64(os, 0);

    write_uint64(os, root_offset);
    write_uint64(os, data.size());

    os.write(data.data(), data.size());
}

void SerialisedHic::read_from_stream(std::istream& is) {
    char read_tag[4] = {};
    is.read(read_tag, sizeof read_tag);

    if (std::memcmp(read_tag, tag, sizeof read_tag) != 0)
        throw std::runtime_error{"invalid file (missing tag)"};

    is.ignore(12);

    root_offset = read_uint64(is);
    auto heap_size = read_uint64(is);
    data.resize(heap_size);
    is.read(data.data(), data.size());

    rw_index = 0;
    object_index = 0;
    object_offsets.clear();
}
