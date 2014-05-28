#pragma once

#include <iosfwd>
#include <cstdint>

void write_uint32(std::ostream& os, std::int32_t x);
void write_uint64(std::ostream& os, std::int64_t x);
std::uint32_t read_uint32(std::istream& is);
std::uint64_t read_uint64(std::istream& is);
