cmake_minimum_required (VERSION 3.0)

project (FortranContainer Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
endif ()
set (CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/fortran_container")

set (version 0.0.1)

set (sources
  "src/linked_list_mod.F90"
  "src/hash_table_mod.F90"
)

add_library (fortran_container SHARED ${sources})
set_property (TARGET fortran_container PROPERTY VERSION ${version})
set_property (TARGET fortran_container PROPERTY SOVERSION 1)

if (RUN_TEST)
  add_subdirectory ("lib/unit-test")
  include_directories (${UNIT_TEST_INCLUDE_DIR})

  add_executable (linked_list_test "src/linked_list_test.F90")
  target_link_libraries (linked_list_test fortran_unit_test fortran_container)

  add_executable (hash_table_test "src/hash_table_test.F90")
  target_link_libraries (hash_table_test fortran_unit_test fortran_container)
endif ()

install (TARGETS fortran_container EXPORT FortranContainerTargets
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)

install (FILES
  "${CMAKE_BINARY_DIR}/.mods/linked_list_mod.mod"
  "${CMAKE_BINARY_DIR}/.mods/hash_table_mod.mod"
  DESTINATION include/fortran_container
)

include (CMakePackageConfigHelpers)
write_basic_package_version_file (
  "${CMAKE_CURRENT_BINARY_DIR}/FortranContainer/FortranContainerConfigVersion.cmake"
  VERSION ${version}
  COMPATIBILITY AnyNewerVersion
)

export (EXPORT FortranContainerTargets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/FortranContainer/FortranContainerTargets.cmake"
)
configure_file (cmake/FortranContainerConfig.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/FortranContainer/FortranContainerConfig.cmake"
)

set (ConfigPackageLocation lib/cmake/FortranContainer)
install (EXPORT FortranContainerTargets
  FILE FortranContainerTargets.cmake
  DESTINATION ${ConfigPackageLocation}
)
install (
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/FortranContainer/FortranContainerConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/FortranContainer/FortranContainerConfigVersion.cmake"
  DESTINATION
    ${ConfigPackageLocation}
)
