cmake_minimum_required(VERSION 3.14)
project(llcl VERSION 0.1.0 LANGUAGES CXX)

add_library(llcl STATIC lib/CommandLine.cpp)
add_library(llcl::llcl ALIAS llcl)

target_compile_features(llcl PUBLIC cxx_std_17)
set_target_properties(llcl PROPERTIES CXX_EXTENSIONS OFF)

target_include_directories(llcl PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

target_compile_options(llcl PRIVATE
  $<$<CXX_COMPILER_ID:GNU,Clang,AppleClang>:-Wall -Wextra -Wpedantic>
  $<$<CXX_COMPILER_ID:MSVC>:/W4>
)

# Install rules
include(GNUInstallDirs)

install(TARGETS llcl
  EXPORT llclTargets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

install(DIRECTORY include/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(EXPORT llclTargets
  FILE llclTargets.cmake
  NAMESPACE llcl::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/llcl
)

include(CMakePackageConfigHelpers)

configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/llclConfig.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/llclConfig.cmake
  INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/llcl
)

write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/llclConfigVersion.cmake
  COMPATIBILITY SameMajorVersion
)

install(FILES
  ${CMAKE_CURRENT_BINARY_DIR}/llclConfig.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/llclConfigVersion.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/llcl
)

if(CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
  enable_testing()

  find_package(GTest REQUIRED)

  add_executable(llcl_test test/CommandLineTest.cpp)

  target_link_libraries(llcl_test PRIVATE llcl GTest::gtest_main GTest::gmock)

  add_test(NAME CommandLineTest COMMAND llcl_test)
endif()
