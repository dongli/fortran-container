cmake_minimum_required(VERSION 3.0)

project(FortranContainer LANGUAGES Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
  if (CMAKE_BUILD_TYPE MATCHES Debug)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -gdwarf -Og -fbacktrace -fcheck=all")
  else ()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2")
  endif ()
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
  if (CMAKE_BUILD_TYPE MATCHES Debug)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -O0 -traceback -fPIC")
  else ()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
  endif ()
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  if (CMAKE_BUILD_TYPE MATCHES Debug)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -O0 -traceback -check all")
  else ()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2")
  endif ()
endif ()
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}")

set(version 0.0.1)

set(source_files
  src/linked_list_mod.F90
  src/hash_table_mod.F90
  src/array_mod.F90
  src/container.F90
)

add_library(fortran_container SHARED ${source_files})
set_property(TARGET fortran_container PROPERTY VERSION ${version})
set_property(TARGET fortran_container PROPERTY SOVERSION 1)

get_directory_property(parent_dir PARENT_DIRECTORY)
if (RUN_TEST AND NOT parent_dir)
  add_subdirectory("lib/unit-test")
  include_directories(${UNIT_TEST_INCLUDE_DIR})

  add_executable(array_test "src/array_test.F90")
  target_link_libraries(array_test fortran_unit_test fortran_container)

  add_executable(linked_list_test "src/linked_list_test.F90")
  target_link_libraries(linked_list_test fortran_unit_test fortran_container)

  add_executable(hash_table_test "src/hash_table_test.F90")
  target_link_libraries(hash_table_test fortran_unit_test fortran_container)
endif ()

install(TARGETS fortran_container EXPORT FortranContainerTargets
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)

install(FILES
  "${CMAKE_BINARY_DIR}/.mods/linked_list_mod.mod"
  "${CMAKE_BINARY_DIR}/.mods/hash_table_mod.mod"
  DESTINATION include/fortran_container
)

include(CMakePackageConfigHelpers)
write_basic_package_version_file (
  "${CMAKE_CURRENT_BINARY_DIR}/FortranContainer/FortranContainerConfigVersion.cmake"
  VERSION ${version}
  COMPATIBILITY AnyNewerVersion
)

export(EXPORT FortranContainerTargets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/FortranContainer/FortranContainerTargets.cmake"
)
configure_file(cmake/FortranContainerConfig.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/FortranContainer/FortranContainerConfig.cmake"
)

set(ConfigPackageLocation lib/cmake/FortranContainer)
install(EXPORT FortranContainerTargets
  FILE FortranContainerTargets.cmake
  DESTINATION ${ConfigPackageLocation}
)
install(
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/FortranContainer/FortranContainerConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/FortranContainer/FortranContainerConfigVersion.cmake"
  DESTINATION
    ${ConfigPackageLocation}
)
