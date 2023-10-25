PROGRAM main

    USE field_mod
    use omp_lib

    INTEGER, PARAMETER :: len = 200000000

    TYPE(field_t) :: afield
    integer :: num_devices,nteams,nthreads
    logical :: initial_device

    num_devices = omp_get_num_devices()
    print *, "Number of available devices", num_devices

    CALL afield%init("AVG", len )

    CALL afield%update_host( 1, len )

    WRITE(*,*) MAXVAL( afield%arr_host )
    WRITE(*,*) MINVAL( afield%arr_host )
    WRITE(*,*) afield%arr_host(1:6)

    CALL afield%finish()

END PROGRAM main
