PROGRAM main

    USE field_mod
    USE gpu_openmp_mod
    use omp_lib

    INTEGER, PARAMETER :: len = 1200 * 64*64*64

    TYPE(field_t) :: afield, bfield, cfield
    integer :: num_devices,nteams,nthreads
    logical :: initial_device

    CALL omp_init()

    CALL afield%init("AAA", len )
    CALL bfield%init("BBB", len )
    CALL cfield%init("CCC", len )

    WRITE(*,*) "MemAllocs done."

    CALL afield%update_host( 1, 9 )
    CALL bfield%update_host( 1, len )
    CALL cfield%update_host( 1, len )

    WRITE(*,*) MAXVAL( afield%arr_host )
    WRITE(*,*) MINVAL( afield%arr_host )
    WRITE(*,*) afield%arr_host(1:10)

    CALL afield%finish()
    CALL bfield%finish()
    CALL cfield%finish()

END PROGRAM main
