MODULE gpu_openmp_mod

    ! DUMMY VERSION

    USE precision_mod, ONLY: intk, realk, real_bytes
    USE omp_lib
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_ptr, c_null_ptr, c_size_t, c_int, c_f_pointer, c_loc

    IMPLICIT NONE(type, external)
    PRIVATE

    INTEGER(intk), PARAMETER :: nchar_name = 16
    INTEGER(intk), PARAMETER :: nchar_desc = 32
    INTEGER(intk), PARAMETER :: nattr_max = 8

    PUBLIC :: allocate_omp_device, deallocate_omp_device, memcp_omp_device_to_host

    interface
        type(c_ptr) function omp_target_alloc(size, device_num) bind(c)
            use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int
            integer(c_size_t), value :: size
            integer(c_int), value :: device_num
        end function omp_target_alloc
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

    SUBROUTINE allocate_omp_device( length, dev_pointer )
        INTEGER(intk), INTENT(in) :: length
        TYPE(c_ptr), INTENT(inout) :: dev_pointer

        ! local variable
        INTEGER(c_size_t) :: size
        INTEGER(c_int), PARAMETER :: device_num = 0
        INTEGER(intk) :: i
        REAL(realk), POINTER :: fptr(:)

        ! size in bytes
        size = real_bytes * length
        WRITE(*,*) size

        ! allocation on the device
        dev_pointer = omp_target_alloc( size, device_num )

        !$omp target is_device_ptr(dev_pointer)
        CALL c_f_pointer(dev_pointer, fptr, [length])

        !$omp teams distribute parallel do
        DO i = 1, length
            ! stupid initialization
            fptr(i) = REAL( i, realk )
        END DO
        !$omp end teams distribute parallel do

        !$omp end target

    END SUBROUTINE allocate_omp_device



    PURE SUBROUTINE deallocate_omp_device( dev_pointer )
        TYPE(c_ptr), INTENT(inout) :: dev_pointer

        ! local variable
        INTEGER(c_int), PARAMETER :: device_num = 0

        ! check of allocation
        CALL omp_target_free( dev_pointer, device_num )

    END SUBROUTINE deallocate_omp_device



    SUBROUTINE memcp_omp_device_to_host( dev_cptr, host_array, length, offset )
        INTEGER(intk), INTENT(in) :: length
        TYPE(c_ptr), INTENT(in) :: dev_cptr
        REAL(realk), INTENT(inout), TARGET :: host_array(length)
        INTEGER(intk), INTENT(in) :: offset

        ! local variable
        INTEGER(c_int), PARAMETER :: device_num = 0
        INTEGER(c_int) :: err
        INTEGER(c_size_t) :: c_len, c_offset
        REAL(realk), POINTER :: host_fptr(:)
        TYPE(c_ptr) :: host_cptr

        ! conversion to c_ptr
        host_fptr => host_array
        host_cptr = C_LOC( host_fptr )

        ! conversion to c_size_t
        c_len = INT( length * real_bytes, c_size_t )
        WRITE(*,*) c_len
        c_offset = INT( offset * real_bytes, c_size_t )
        WRITE(*,*) c_offset

        err = omp_target_memcpy( host_cptr, dev_cptr, c_len, c_offset, c_offset, 1, device_num )

    END SUBROUTINE memcp_omp_device_to_host



    ! int omp_target_memcpy(
    !     void *dst,
    !     const void *src,
    !     size_t length,
    !     size_t dst_offset,
    !     size_t src_offset,
    !     int dst_device_num,
    !     int src_device_num
    !   );



END MODULE gpu_openmp_mod
