PROGRAM main

    USE field_mod
    USE precision_mod
    USE gpu_openmp_mod

    use omp_lib
    use mpi_f08
    USE, INTRINSIC :: ISO_C_BINDING

    implicit none

    INTEGER, PARAMETER :: len = 600 * 64*64*64

    TYPE(field_t) :: afield, bfield, cfield
    TYPE(MPI_STATUS) :: stat
    integer :: num_devices,nteams,nthreads
    integer :: i, rank, size, required, provided, tag, ierr, count
    logical :: initial_device

    REAL(realk), POINTER :: fptr(:)
    TYPE(c_ptr) :: cptr

    NULLIFY( fptr )

    required = MPI_THREAD_MULTIPLE
    CALL MPI_Init_thread( required, provided )
    IF ( provided /= MPI_THREAD_MULTIPLE ) THEN
        WRITE(*,*) "weak MPI..."
    END IF

    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
    CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

    CALL init_precision()
    CALL omp_init()

    CALL afield%init("U", len )
    CALL bfield%init("V", len )
    CALL cfield%init("W", len )
    WRITE(*,*) "MemAllocs done."

    DO i = 1, len
        afield%arr_host(i) = real(i,realk)
    END DO
    WRITE(*,*) "A locally initialized."

    CALL afield%update_device( 1, len )
    afield%arr_host = 0.0
    CALL afield%update_host( 1, len )
    WRITE(*,*) "Back and forth finished."

    tag = 123
    count = len
    IF ( rank == 0 ) THEN
        ! GPU-aware MPI can now use the device pointer
        cptr = afield%arr_device
        CALL c_f_pointer(cptr, fptr, [len])
        CALL MPI_Send( fptr, count, &
        mglet_mpi_real, 1, tag, MPI_COMM_WORLD, ierr )
    END IF

    IF ( rank == 1 ) THEN
        ! GPU-aware MPI can now use the device pointer
        cptr = bfield%arr_device
        CALL c_f_pointer(cptr, fptr, [len])
        CALL MPI_Recv( fptr, count, &
        MPI_REAL, 0, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr )
    END IF

    CALL bfield%update_host( 1, len )
    CALL cfield%update_host( 1, len )

    WRITE(*,*) MAXVAL( afield%arr_host )
    WRITE(*,*) MINVAL( afield%arr_host )
    WRITE(*,*) afield%arr_host(1:10)

    WRITE(*,*) MAXVAL( bfield%arr_host )
    WRITE(*,*) MINVAL( bfield%arr_host )
    WRITE(*,*) bfield%arr_host(1:10)

    CALL afield%finish()
    CALL bfield%finish()
    CALL cfield%finish()

    CALL MPI_Finalize( ierr )

END PROGRAM main
