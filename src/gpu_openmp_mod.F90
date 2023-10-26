MODULE gpu_openmp_mod

    ! DUMMY VERSION

    USE precision_mod, ONLY: intk, realk, real_bytes
    USE omp_lib
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_ptr, c_null_ptr, c_size_t, c_int, c_f_pointer, c_loc, c_associated

    IMPLICIT NONE(type, external)
    PRIVATE

    INTEGER(intk), PARAMETER :: nchar_name = 16
    INTEGER(intk), PARAMETER :: nchar_desc = 32
    INTEGER(intk), PARAMETER :: nattr_max = 8
    INTEGER(intk) :: my_device = -1
    INTEGER(intk) :: my_host = -1

    PUBLIC :: omp_init, allocate_omp_device, deallocate_omp_device, &
              memcp_omp_device_to_host, memcp_omp_host_to_device

    interface
        type(c_ptr) function omp_target_alloc(size, device_num) bind(c)
            use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int
            integer(c_size_t), value :: size
            integer(c_int), value :: device_num
        end function omp_target_alloc
    end interface

    interface
        type(c_ptr) function omp_get_mapped_ptr(ptr, device_num) bind(c)
            use, intrinsic :: iso_c_binding, only : c_ptr, c_int
            type(c_ptr), value :: ptr
            integer(c_int), value :: device_num
        end function omp_get_mapped_ptr
    end interface

    interface
        pure subroutine omp_target_free(dev_pointer, device_num) bind(c)
            use, intrinsic :: iso_c_binding, only : c_ptr, c_int
            type(c_ptr), value :: dev_pointer
            integer(c_int), value :: device_num
        end subroutine omp_target_free
    end interface

    interface
        integer(c_int) function omp_target_memcpy(dst, src, length, dst_off, src_off, dst_dev, src_dev) bind(c)
            use iso_c_binding
            type(c_ptr), value :: dst
            type(c_ptr), value, intent(in) :: src
            integer(kind=c_size_t), value, intent(in) :: length
            integer(kind=c_size_t), value, intent(in) :: dst_off
            integer(kind=c_size_t), value, intent(in) :: src_off
            integer(kind=c_int), value, intent(in) :: dst_dev
            integer(kind=c_int), value, intent(in) :: src_dev
        end function omp_target_memcpy
    end interface



CONTAINS

    SUBROUTINE omp_init( )
        INTEGER(intk) :: ndevice
        INTEGER(intk), PARAMETER :: myid = 0

        ndevice = omp_get_num_devices()
        WRITE(*,*) "ndevice = ", ndevice
        ! consider moving to block allocation
        my_device = mod( myid, ndevice )
        CALL omp_set_default_device( my_device )
        ! activates the device (initialization on demand)
        !$omp target
        !$omp end target
        WRITE(*,*) "host device = ", omp_get_initial_device()
        my_host = omp_get_initial_device()
        WRITE(*,*) "target device = ", omp_get_default_device()
        my_device = omp_get_default_device()

    END SUBROUTINE omp_init



    SUBROUTINE allocate_omp_device( length, dev_pointer )
        INTEGER(intk), INTENT(in) :: length
        TYPE(c_ptr), INTENT(out) :: dev_pointer

        ! local variable
        INTEGER(c_size_t) :: size
        INTEGER(intk) :: i
        REAL(realk), POINTER :: fptr(:)

        ! important
        NULLIFY( fptr )

        ! size in bytes
        size = INT(real_bytes,c_size_t) * INT(length,c_size_t)

        ! allocation on the device
        IF ( .not. c_associated(dev_pointer) ) THEN
            dev_pointer = omp_target_alloc( size, my_device )
        ELSE
            WRITE(*,*) "ERROR: device pointer is already set"
        END IF

        !$omp target device(my_device) is_device_ptr(dev_pointer)
        CALL c_f_pointer(dev_pointer, fptr, [length])
        !$omp teams distribute parallel do
        DO i = 1, length
            fptr(i) = REAL( 0.0, realk )
        END DO
        !$omp end teams distribute parallel do
        !$omp end target

    END SUBROUTINE allocate_omp_device



    PURE SUBROUTINE deallocate_omp_device( dev_pointer )
        TYPE(c_ptr), INTENT(inout) :: dev_pointer

        !  $omp target exit data map(delete:sendbuf,recvbuf)

        ! check of allocation
        ! CALL omp_target_free( dev_pointer, my_device )

    END SUBROUTINE deallocate_omp_device



    SUBROUTINE memcp_omp_device_to_host( dev_cptr, host_array, length, offset )
        INTEGER(intk), INTENT(in) :: length
        TYPE(c_ptr), INTENT(in) :: dev_cptr
        REAL(realk), INTENT(inout), TARGET :: host_array(length)
        INTEGER(intk), INTENT(in) :: offset

        ! local variable
        INTEGER(c_int) :: err
        INTEGER(c_size_t) :: c_len, c_offset
        REAL(realk), POINTER :: host_fptr(:)
        TYPE(c_ptr) :: host_cptr

        ! conversion to c_ptr
        NULLIFY( host_fptr )
        host_fptr => host_array
        host_cptr = C_LOC( host_fptr )

        ! conversion to c_size_t
        c_len = INT( length * real_bytes, c_size_t )
        c_offset = INT( offset * real_bytes, c_size_t )

        ! copy back from the device
        err = omp_target_memcpy( host_cptr, dev_cptr, &
            c_len, c_offset, c_offset, my_host, my_device )

        IF ( err /= 0 ) THEN
            WRITE(*,*) "ERROR: copy to host failed"
        END IF

    END SUBROUTINE memcp_omp_device_to_host


    SUBROUTINE memcp_omp_host_to_device( host_array, dev_cptr, length, offset )
        INTEGER(intk), INTENT(in) :: length
        TYPE(c_ptr), INTENT(in) :: dev_cptr
        REAL(realk), INTENT(inout), TARGET :: host_array(length)
        INTEGER(intk), INTENT(in) :: offset

        ! local variable
        INTEGER(c_int) :: err
        INTEGER(c_size_t) :: c_len, c_offset
        REAL(realk), POINTER :: host_fptr(:)
        TYPE(c_ptr) :: host_cptr

        ! conversion to c_ptr
        NULLIFY( host_fptr )
        host_fptr => host_array
        host_cptr = C_LOC( host_fptr )

        ! conversion to c_size_t
        c_len = INT( length * real_bytes, c_size_t )
        c_offset = INT( offset * real_bytes, c_size_t )

        ! copy back from the device
        err = omp_target_memcpy( dev_cptr, host_cptr,  &
            c_len, c_offset, c_offset, my_device, my_host )

        IF ( err /= 0 ) THEN
            WRITE(*,*) "ERROR: copy to device failed"
        END IF

    END SUBROUTINE memcp_omp_host_to_device

END MODULE gpu_openmp_mod
