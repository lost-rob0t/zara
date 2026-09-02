cmake_minimum_required(VERSION 3.22.1)
project(zara_trealla LANGUAGES C)

set(ZARA_TREALLA_SOURCE_DIR "" CACHE PATH "Pinned Trealla source tree")
set(ZARA_TREALLA_LIBRARY_ROOT "" CACHE PATH "Per-ABI pinned Trealla libraries")

if(NOT ZARA_TREALLA_SOURCE_DIR)
    message(FATAL_ERROR "ZARA_TREALLA_SOURCE_DIR is required")
endif()

if(NOT EXISTS "${ZARA_TREALLA_SOURCE_DIR}/src/trealla.h")
    message(FATAL_ERROR "ZARA_TREALLA_SOURCE_DIR must contain src/trealla.h")
endif()

if(NOT ANDROID_ABI)
    message(FATAL_ERROR "ANDROID_ABI is required")
endif()

set(ZARA_TREALLA_LIBRARY "${ZARA_TREALLA_LIBRARY_ROOT}/${ANDROID_ABI}/libtrealla.a")
if(NOT ZARA_TREALLA_LIBRARY_ROOT OR NOT EXISTS "${ZARA_TREALLA_LIBRARY}")
    message(FATAL_ERROR "missing pinned Trealla library for ${ANDROID_ABI}: ${ZARA_TREALLA_LIBRARY}")
endif()

add_library(trealla STATIC IMPORTED)
set_target_properties(trealla PROPERTIES
    IMPORTED_LOCATION "${ZARA_TREALLA_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${ZARA_TREALLA_SOURCE_DIR}/src"
)

add_library(zara_trealla SHARED zara_trealla_jni.c)
target_compile_features(zara_trealla PRIVATE c_std_11)
target_compile_options(zara_trealla PRIVATE -Wall -Wextra -Werror)
target_link_libraries(zara_trealla PRIVATE trealla)
