#include "stream_dispenser.hpp"
#include "configuration_read.hpp"
#include <sstream>
#include <iomanip>
#include <utility>

StreamDispenser::StreamDispenser(std::string base_name, std::string extension)
    : base_name_(std::move(base_name))
    , extension_(std::move(extension)) {}

OStreamPtr StreamDispenser::get_next() {
    std::ostringstream name;
    name << configuration::get_output_dir() << '/' << base_name_ << '_';
    name << std::setw(5) << std::setfill('0') << next_file_number_++;
    name << '.' << extension_;
    return open_file(name.str());
}
