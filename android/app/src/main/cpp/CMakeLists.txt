cmake_minimum_required(VERSION 3.22.1)
project(zara_trealla LANGUAGES C)

set(ZARA_TREALLA_SOURCE_DIR "" CACHE PATH "Pinned Trealla source tree")
set(ZARA_TREALLA_LIBRARY "" CACHE FILEPATH "Pinned Trealla static/shared library")

if(NOT ZARA_TREALLA_SOURCE_DIR)
    message(FATAL_ERROR "ZARA_TREALLA_SOURCE_DIR is required")
endif()

if(NOT EXISTS "${ZARA_TREALLA_SOURCE_DIR}/src/trealla.h")
    message(FATAL_ERROR "ZARA_TREALLA_SOURCE_DIR must contain src/trealla.h")
endif()

if(NOT ZARA_TREALLA_LIBRARY OR NOT EXISTS "${ZARA_TREALLA_LIBRARY}")
    message(FATAL_ERROR "ZARA_TREALLA_LIBRARY must name a pinned Trealla library")
endif()

add_library(trealla UNKNOWN IMPORTED)
set_target_properties(trealla PROPERTIES
    IMPORTED_LOCATION "${ZARA_TREALLA_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${ZARA_TREALLA_SOURCE_DIR}/src"
)

add_library(zara_trealla SHARED zara_trealla_jni.c)
target_compile_features(zara_trealla PRIVATE c_std_11)
target_compile_options(zara_trealla PRIVATE -Wall -Wextra -Werror)
target_link_libraries(zara_trealla PRIVATE trealla)
