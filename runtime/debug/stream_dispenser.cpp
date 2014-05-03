#include "debug/stream_dispenser.hpp"
#include <sstream>
#include <iomanip>
#include <utility>

StreamDispenser::StreamDispenser(std::string base_name, std::string extension)
    : base_name_(std::move(base_name))
    , extension_(std::move(extension)) {}

void StreamDispenser::set_output_dir(std::string str) {
    directory_ = std::move(str);
}

OStreamPtr StreamDispenser::get_next() {
    std::ostringstream name;
    if (!directory_.empty())
        name << directory_ << '/';
    name << base_name_ << '_';
    name << std::setw(5) << std::setfill('0') << next_file_number_++;
    name << '.' << extension_;
    return open_file(name.str());
}
