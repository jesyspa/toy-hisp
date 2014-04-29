#pragma once

#include <ostream>
#include <memory>

// Encapsulates a pointer to a stream.  Guaranteed to point to a valid stream, and will clean up after itself.
class OStreamPtr {
    using deleter_func = void(std::ostream*);
    std::unique_ptr<std::ostream, deleter_func*> stream_;

    OStreamPtr() = delete;
    OStreamPtr(std::ostream* stream, deleter_func* deleter)
        : stream_(stream, deleter) {}

public:
    std::ostream& operator*() const { return *stream_; }

    std::ostream* operator->() const { return stream_.get(); }

    void swap(OStreamPtr& other) { stream_.swap(other.stream_); }

    friend OStreamPtr open_file(std::string filename);

    // TODO: Come up with a better name for this.
    friend OStreamPtr take_pointer(std::ostream& os);
};

OStreamPtr open_file(std::string filename);
OStreamPtr take_pointer(std::ostream& os);
