MODULE field_mod

    USE precision_mod

    IMPLICIT NONE(type, external)
    PRIVATE

    INTEGER(intk), PARAMETER :: nchar_name = 16
    INTEGER(intk), PARAMETER :: nchar_desc = 32
    INTEGER(intk), PARAMETER :: nattr_max = 8

    TYPE :: field_t

        LOGICAL, PRIVATE :: is_init = .FALSE.
        CHARACTER(len=nchar_name) :: name = REPEAT(" ", nchar_name)
        REAL(realk), ALLOCATABLE :: arr(:)
        INTEGER(intk) :: length

    CONTAINS

        PROCEDURE :: init
        PROCEDURE :: finish
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

        ALLOCATE(this%arr(this%length))
        this%arr = 0.0

    END SUBROUTINE init


    ELEMENTAL SUBROUTINE finish(this)
        CLASS(field_t), INTENT(inout) :: this

        IF (.NOT. this%is_init) RETURN

        this%is_init = .FALSE.
        this%name = REPEAT(" ", nchar_name)
        this%length = 0

        DEALLOCATE(this%arr)

    END SUBROUTINE finish


    ELEMENTAL SUBROUTINE destructor(this)
        TYPE(field_t), INTENT(inout) :: this

        CALL this%finish()
    END SUBROUTINE destructor

END MODULE field_mod
