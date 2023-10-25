# Adding to library
add_library(mgletlib SHARED
    timeloop_mod.F90
    field_mod.F90
    precision_mod.F90
    gpu_openmp_mod.F90
)

# set(CMAKE_SHARED_LINKER_FLAGS -lgfortran -fopenmp -foffload=nvptx-none )
# set(CMAKE_PRIVATE_LINKER_FLAGS -lgfortran -fopenmp -foffload=nvptx-none )

set_source_files_properties(timeloop_mod.F90 PROPERTIES COMPILE_FLAGS
    $<$<STREQUAL:"${CMAKE_Fortran_COMPILER_ID}","GNU">:-fall-intrinsics>
)

set_source_files_properties(gpu_openmp_mod.F90 PROPERTIES LINK_FLAGS
    $<$<STREQUAL:"${CMAKE_Fortran_COMPILER_ID}","GNU">:-gfortran>
)

target_include_directories(mgletlib
    PRIVATE ${MPI_Fortran_INCLUDE_PATH}
    PRIVATE ${OpenMP_Fortran_INCLUDE_PATH}
)
target_link_libraries(mgletlib
    PRIVATE ${MPI_Fortran_LIBRARIES}
    PRIVATE ${OpenMP_Fortran_LIBRARIES}
)

# Executable
add_executable(mglet
    mglet.F90
)

# set(CMAKE_SHARED_LINKER_FLAGS -lgfortran -fopenmp -foffload=nvptx-none )
# set(CMAKE_PRIVATE_LINKER_FLAGS -lgfortran -fopenmp -foffload=nvptx-none )

target_include_directories(mglet
    PRIVATE ${MPI_Fortran_INCLUDE_PATH}
    PRIVATE ${OpenMP_Fortran_INCLUDE_PATH}
)
target_link_libraries(mglet
    PRIVATE mgletlib
    PRIVATE ${MPI_Fortran_LIBRARIES}
    PRIVATE ${OpenMP_Fortran_LIBRARIES}
)


# Intel must use the Fortran compiler to link properly
if (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    set_property(TARGET mglet PROPERTY LINKER_LANGUAGE Fortran)
endif()
