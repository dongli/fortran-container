cmake_minimum_required (VERSION 3.0)

project (FortranContainer Fortran)

set (version 0.0.1)

#if (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
#  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
#endif ()

set (sources
  "src/linked_list_mod.F90"
  "src/hash_table_mod.F90"
)

add_library (fortran_container ${sources})
set_property (TARGET fortran_container PROPERTY VERSION ${version})
set_property (TARGET fortran_container PROPERTY SOVERSION 1)

if (RUN_TEST)
  add_subdirectory ("lib/unit-test")
  include_directories ("${PROJECT_BINARY_DIR}/fortran_unit_test")

  add_executable (linked_list_test "src/linked_list_test.F90")
  target_link_libraries (linked_list_test fortran_unit_test fortran_container)

  add_executable (hash_table_test "src/hash_table_test.F90")
  target_link_libraries (hash_table_test fortran_unit_test fortran_container)
endif ()
