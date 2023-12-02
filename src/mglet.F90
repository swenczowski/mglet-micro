PROGRAM main

    USE field_mod
    USE precision_mod
    USE gpu_openmp_mod
    USE gpu_stencil_mod

    use omp_lib
    use mpi_f08
    USE, INTRINSIC :: ISO_C_BINDING

    implicit none

    INTEGER, PARAMETER :: ngrid = 2*200
    INTEGER, PARAMETER :: nx = 32
    INTEGER, PARAMETER :: ny = 32
    INTEGER, PARAMETER :: nz = 32
    INTEGER, PARAMETER :: len = ngrid * nx * ny * nz

    TYPE(field_t) :: afield, bfield, cfield, dfield, efield, ffield
    TYPE(MPI_STATUS) :: stat
    integer :: num_devices,nteams,nthreads
    integer :: i, rank, size, required, provided, tag, ierr, count
    logical :: initial_device
    real :: start, finish

    REAL(realk), POINTER :: fptr(:)
    TYPE(c_ptr) :: cptr

    NULLIFY( fptr )

    ! launching MPI with threading support
    required = MPI_THREAD_MULTIPLE
    CALL MPI_Init_thread( required, provided )
    IF ( provided /= MPI_THREAD_MULTIPLE ) THEN
        WRITE(*,*) "weak MPI..."
    END IF

    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
    CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

    CALL init_precision()
    CALL omp_init()

    ! allocating fields (local and device)
    CALL afield%init("U", len )
    CALL bfield%init("V", len )
    CALL cfield%init("W", len )
    CALL dfield%init("T1", len )
    CALL efield%init("T2", len )
    CALL ffield%init("T3", len )
    WRITE(*,*) "MemAllocs done."



    !!! TEST 1 --- memcpy check ---

    ! filling one local field
    DO i = 1, len
        afield%arr_host(i) = real(i,realk)
    END DO

    ! pushing to device, overwriting on host, getting from device
    CALL afield%update_device( 1, len )
    afield%arr_host = 0.0
    CALL afield%update_host( 1, len )

    ! checking for difference
    DO i = 1, len
        IF ( afield%arr_host(i) /= real(i,realk) ) THEN 
            WRITE(*,*) "Difference(a-a) = ", afield%arr_host(i) /= real(i,realk)
        END IF
    END DO
    WRITE(*,*) "Back and forth finished."



    !!! TEST 2 --- MPI check ---

    ! communication with CUDA-aware MPI on casted device pointers
    IF ( size > 1 ) THEN
        tag = 123
        count = len
        IF ( rank == 0 ) THEN
            cptr = afield%arr_device
            CALL c_f_pointer(cptr, fptr, [len])
            CALL MPI_Send( fptr, count, &
            mglet_mpi_real, 1, tag, MPI_COMM_WORLD, ierr )
        END IF
        IF ( rank == 1 ) THEN
            cptr = bfield%arr_device
            CALL c_f_pointer(cptr, fptr, [len])
            CALL MPI_Recv( fptr, count, &
            mglet_mpi_real, 0, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr )
        END IF
    END IF

    ! checking for difference
    IF ( rank == 1 ) THEN
        CALL bfield%update_host( 1, len )
        DO i = 1, len
            IF ( bfield%arr_host(i) /= real(i,realk) ) THEN 
                WRITE(*,*) "Difference(b-b) = ", bfield%arr_host(i) /= real(i,realk)
            END IF
        END DO
    END IF

    WRITE(*,*) "MPI check finished."



    !!! TEST 3 --- kernel check ---

    ! version CPU) : call a stencil routine
    call cpu_time(start)
    DO i = 1, 3
        CALL stencil_version_cpu( afield%arr_host, ffield%arr_host, ngrid, nx, ny, nz )
    END DO
    call cpu_time(finish)
    WRITE(*,*) finish-start


    !$omp target enter data map( to : afield, efield )

    call cpu_time(start)
    DO i = 1, 3
        CALL stencil_version_gpu( afield%arr_host, efield%arr_host, ngrid, nx, ny, nz )
    END DO
    call cpu_time(finish)
    WRITE(*,*) finish-start

    !$omp target exit data map( from : afield, efield )


    ! version A) : call a stencil routine ("massive collapsing")
    call cpu_time(start)
    DO i = 1, 3
        CALL stencil_version_a( afield%arr_device, cfield%arr_device, ngrid, nx, ny, nz )
    END DO
    call cpu_time(finish)
    WRITE(*,*) finish-start
    CALL cfield%update_host( 1, len )

    ! version B) : call a stencil routine ("teams and threads")
    call cpu_time(start)
    DO i = 1, 3
        CALL stencil_version_b( afield%arr_device, dfield%arr_device, ngrid, nx, ny, nz )
    END DO
    call cpu_time(finish)
    WRITE(*,*) finish-start
    CALL dfield%update_host( 1, len )

    ! version C) : call a stencil routine ("teams and stencil routine with threads")
    call cpu_time(start)
    DO i = 1, 3
        CALL stencil_version_c( afield%arr_device, efield%arr_device, ngrid, nx, ny, nz )
    END DO
    call cpu_time(finish)
    WRITE(*,*) finish-start
    CALL efield%update_host( 1, len )


    ! checking for difference
    DO i = 1, len
        IF ( ffield%arr_host(i) /= cfield%arr_host(i) ) THEN 
            WRITE(*,*) "Difference(c-d) = ", ffield%arr_host(i) - cfield%arr_host(i)
        END IF
        IF ( ffield%arr_host(i) /= dfield%arr_host(i) ) THEN 
            WRITE(*,*) "Difference(c-d) = ", ffield%arr_host(i) - dfield%arr_host(i)
        END IF
        IF ( ffield%arr_host(i) /= efield%arr_host(i) ) THEN 
            WRITE(*,*) "Difference(c-e) = ", ffield%arr_host(i) - efield%arr_host(i)
        END IF
    END DO

    WRITE(*,*) "Kernel check finished."




    CALL afield%finish()
    CALL bfield%finish()
    CALL cfield%finish()
    CALL dfield%finish()
    CALL efield%finish()

    CALL MPI_Finalize( ierr )

END PROGRAM main
