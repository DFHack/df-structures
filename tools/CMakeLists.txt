option(BUILD_XMLDUMP "Build utilities that dump df-structures information (for CI)" OFF)
if(BUILD_XMLDUMP)
    find_package(Perl REQUIRED)
    find_package(Python 3 REQUIRED)
    file(GLOB XML_FILES ../df.*.xml)
    set(XML_DUMP_GENERATOR "${CMAKE_CURRENT_SOURCE_DIR}/generate.py")
    add_custom_command(OUTPUT dump-struct-sizes.cpp
        COMMAND ${Python_EXECUTABLE} "${XML_DUMP_GENERATOR}"
            --template dump-struct-sizes.cpp.in
            --output "${CMAKE_CURRENT_BINARY_DIR}/dump-struct-sizes.cpp"
            --perl "${PERL_EXECUTABLE}"
        VERBATIM
        DEPENDS ${XML_FILES} "${XML_DUMP_GENERATOR}"
    )
    add_executable(xml-dump-struct-sizes dump-struct-sizes.cpp)
    add_dependencies(xml-dump-struct-sizes generate_headers)
endif()
