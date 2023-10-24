MODULE timeloop_mod

    USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_char, c_double, c_ptr, c_long_long
    USE HDF5
    USE MPI_f08

    IMPLICIT NONE(type, external)
    PRIVATE

    INTEGER :: year = 6

    PUBLIC :: init_timeloop, finish_timeloop, timeloop

CONTAINS
    SUBROUTINE init_timeloop()

        WRITE(*,*) "Hello"

    END SUBROUTINE init_timeloop


    SUBROUTINE finish_timeloop()

        WRITE(*,*) "Bye bye"

    END SUBROUTINE finish_timeloop


    SUBROUTINE timeloop()

        WRITE(*,*) "Busy", year

    END SUBROUTINE timeloop


END MODULE timeloop_mod
