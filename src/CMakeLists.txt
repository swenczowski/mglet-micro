# Adding to library
add_library(mgletlib SHARED
    timeloop_mod.F90
    field_mod.F90
    precision_mod.F90
)
set_source_files_properties(timeloop_mod.F90 PROPERTIES COMPILE_FLAGS
    $<$<STREQUAL:"${CMAKE_Fortran_COMPILER_ID}","GNU">:-fall-intrinsics>
)
target_include_directories(mgletlib
    PRIVATE ${MPI_Fortran_INCLUDE_PATH}
    PRIVATE ${HDF5_Fortran_INCLUDE_DIRS}
)
target_link_libraries(mgletlib
    PRIVATE ${MPI_Fortran_LIBRARIES}
    PRIVATE ${HDF5_Fortran_LIBRARIES}
)

# Executable
add_executable(mglet
    mglet.F90
)
target_include_directories(mglet
    PRIVATE ${MPI_Fortran_INCLUDE_PATH}
)
target_link_libraries(mglet
    PRIVATE mgletlib
    PRIVATE ${MPI_Fortran_LIBRARIES}
)


# Intel must use the Fortran compiler to link properly
if (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    set_property(TARGET mglet PROPERTY LINKER_LANGUAGE Fortran)
endif()
