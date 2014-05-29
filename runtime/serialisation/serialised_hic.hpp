#pragma once

#include "hisp/object.hpp"
#include <cstdint>
#include <vector>
#include <map>
#include <type_traits>
#include <boost/bimap.hpp>

class SerialisedHic {
    static_assert(sizeof(std::uint64_t) == 8, "unsupported architecture");
    static_assert(sizeof(std::uint32_t) == 4, "unsupported architecture");

    static char const* const tag;
    std::size_t root_offset;
    std::vector<char> data;
    using offset_map = boost::bimap<Ref, std::size_t>;
    offset_map object_offsets;
    std::size_t rw_index;
    std::size_t object_index;

public:
    SerialisedHic();

    template <typename POD>
    void write(POD const& pod);

    template <typename POD>
    void read(POD& pod);

    template <typename POD>
    POD read_direct();

    void write_bytes(char const* ptr, std::size_t bytes);
    void read_bytes(char* ptr, std::size_t bytes);

    void ignore(std::size_t bytes);
    void advance(std::size_t bytes);
    void seek_begin();

    void start_object();
    void end_object(Ref ptr);

    std::size_t size() const;
    bool at_top() const;

    Ref from_offset(std::size_t offset) const;
    std::size_t to_offset(Ref obj) const;

    void set_root(Ref obj);
    Ref get_root() const;

    void write_to_stream(std::ostream& os) const;
    void read_from_stream(std::istream& is);
};

template <typename POD>
void SerialisedHic::write(POD const& pod) {
    static_assert(!std::is_pointer<POD>::value, "serializing pointer");
    write_bytes(reinterpret_cast<char const*>(&pod), sizeof(pod));
}

template <typename POD>
void SerialisedHic::read(POD& pod) {
    static_assert(!std::is_pointer<POD>::value, "deserializing pointer");
    read_bytes(reinterpret_cast<char*>(&pod), sizeof(pod));
}

template <typename POD>
POD SerialisedHic::read_direct() {
    POD pod;
    read(pod);
    return pod;
}
