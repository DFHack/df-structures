option(BUILD_XMLDUMP "Build utilities that dump df-structures information (for CI)" OFF)
option(INSTALL_XMLDUMP "Install utilities that dump df-structures information (for CI)" OFF)
if(BUILD_XMLDUMP)
    find_package(Perl REQUIRED)
    find_package(Python 3 REQUIRED)

    INCLUDE(FetchContent)
    FetchContent_Declare(
        fmt
        GIT_REPOSITORY https://github.com/fmtlib/fmt.git
        GIT_TAG 790b9389ae99c4ddebdd2736a8602eca1fec684e # 12.1.0 + bugfix for MSVC warning + build time improvements
    )
    FetchContent_MakeAvailable(fmt)
    set(FMTLIB fmt)
    add_definitions("-DUSE_FMTLIB")

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
    target_link_libraries(xml-dump-type-sizes PRIVATE ${FMTLIB})
    if(INSTALL_XMLDUMP)
        install(TARGETS xml-dump-type-sizes DESTINATION .)
    endif()
endif()


