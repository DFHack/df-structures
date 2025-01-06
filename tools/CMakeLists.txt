option(BUILD_XMLDUMP "Build utilities that dump df-structures information (for CI)" OFF)
option(INSTALL_XMLDUMP "Install utilities that dump df-structures information (for CI)" OFF)
if(BUILD_XMLDUMP)
    find_package(Perl REQUIRED)
    find_package(Python 3 REQUIRED)
    file(GLOB XML_FILES ../df.*.xml)
    set(TYPE_DUMP_GENERATOR "${CMAKE_CURRENT_SOURCE_DIR}/generate-type-dump.py")
    add_custom_command(OUTPUT dump-type-sizes.cpp
        COMMAND ${Python_EXECUTABLE} "${TYPE_DUMP_GENERATOR}"
            --template "${CMAKE_CURRENT_SOURCE_DIR}/dump-type-sizes.cpp.in"
            --output "${CMAKE_CURRENT_BINARY_DIR}/dump-type-sizes.cpp"
            --perl "${PERL_EXECUTABLE}"
        VERBATIM
        DEPENDS ${XML_FILES} "${TYPE_DUMP_GENERATOR}"
    )
    add_executable(xml-dump-type-sizes dump-type-sizes.cpp)
    add_dependencies(xml-dump-type-sizes generate_headers)
    target_include_directories(xml-dump-type-sizes PRIVATE ${dfapi_SOURCE_DIR}/include)
    if(INSTALL_XMLDUMP)
        install(TARGETS xml-dump-type-sizes DESTINATION .)
    endif()
endif()
