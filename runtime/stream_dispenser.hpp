#pragma once

#include "stream_ptr.hpp"
#include <string>

// Utility class for getting streams on-demand.
class StreamDispenser {
    // TODO: Allow for multiple implementations.
    // Can currently only create files of the form basename_num.ext
    int next_file_number_ = 0;
    std::string base_name_;
    std::string extension_;

public:
    // We allow moving, but not copying, to prevent accidantally overwriting files.
    // This isn't a fool-proof way, but it should save us some trouble.
    StreamDispenser(StreamDispenser const&) = delete;
    StreamDispenser& operator=(StreamDispenser const&) = delete;
    StreamDispenser(StreamDispenser&&) = default;
    StreamDispenser& operator=(StreamDispenser&&) = default;

    StreamDispenser(std::string base_name, std::string extension);

    OStreamPtr get_next();
};
