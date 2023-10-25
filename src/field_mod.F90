MODULE field_mod

    ! DUMMY VERSION

    USE precision_mod
    USE gpu_openmp_mod
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_ptr, c_null_ptr

    IMPLICIT NONE(type, external)
    PRIVATE

    INTEGER(intk), PARAMETER :: nchar_name = 16
    INTEGER(intk), PARAMETER :: nchar_desc = 32
    INTEGER(intk), PARAMETER :: nattr_max = 8

    TYPE :: field_t

        LOGICAL, PRIVATE :: is_init = .FALSE.
        CHARACTER(len=nchar_name) :: name = REPEAT(" ", nchar_name)
        INTEGER(intk) :: length

        REAL(realk), ALLOCATABLE :: arr_host(:)
        TYPE(c_ptr) :: arr_device = c_null_ptr

    CONTAINS

        PROCEDURE :: init
        PROCEDURE :: finish
        PROCEDURE :: update_host
        FINAL :: destructor

    END TYPE field_t

    PUBLIC :: field_t

CONTAINS

    SUBROUTINE init(this, name, length)
        CLASS(field_t), INTENT(out) :: this
        CHARACTER(len=*), INTENT(in) :: name
        INTEGER(intk), INTENT(in) :: length

        this%is_init = .TRUE.
        IF (LEN_TRIM(name) > nchar_name) WRITE(*,*) 'Name to long!'
        this%name = name
        this%length = length

        ! allocation on host
        ALLOCATE(this%arr_host(this%length))
        this%arr_host = 0.0

        ! allocation on device
        CALL allocate_omp_device( this%length, this%arr_device )

    END SUBROUTINE init


    ELEMENTAL SUBROUTINE finish(this)
        CLASS(field_t), INTENT(inout) :: this

        IF (.NOT. this%is_init) RETURN

        this%is_init = .FALSE.
        this%name = REPEAT(" ", nchar_name)
        this%length = 0

        ! deallocation on host
        DEALLOCATE(this%arr_host)

        ! deallocation on device
        CALL deallocate_omp_device( this%arr_device )

    END SUBROUTINE finish


    SUBROUTINE update_host(this, start, stop)
        CLASS(field_t), INTENT(inout) :: this
        INTEGER(intk), INTENT(in) :: start
        INTEGER(intk), INTENT(in) :: stop

        IF (.NOT. this%is_init) RETURN


        CALL memcp_omp_device_to_host( this%arr_device, this%arr_host, stop-start+1, start-1 )

    END SUBROUTINE update_host


    ELEMENTAL SUBROUTINE destructor(this)
        TYPE(field_t), INTENT(inout) :: this

        CALL this%finish()

    END SUBROUTINE destructor

END MODULE field_mod
