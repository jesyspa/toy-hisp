set(RUNTIME_SOURCE_FILES debug/debugger.cpp debug/stream_ptr.cpp hisp/eval.cpp main.cpp memory/serialisation.cpp
    memory/space_iterators.cpp debug/stream_dispenser.cpp hisp/builtins.cpp hisp/object.cpp
    memory/garbage_collection.cpp memory/space.cpp memory/stack.cpp)

include_directories(.)
add_executable(hisp ${RUNTIME_SOURCE_FILES})
target_link_libraries(hisp boost_program_options)
