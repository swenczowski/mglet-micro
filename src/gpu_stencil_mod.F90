MODULE gpu_stencil_mod

    ! DUMMY VERSION

    USE field_mod, ONLY: field_t
    USE precision_mod, ONLY: intk, realk, real_bytes
    USE omp_lib
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_ptr, c_null_ptr, c_size_t, c_int, c_f_pointer, c_loc, c_associated

    IMPLICIT NONE(type, external)

    PUBLIC :: stencil_version_a, stencil_version_b, stencil_version_c

CONTAINS

    SUBROUTINE stencil_version_a( dev_fi, dev_fo, ngr, nx, ny, nz )
        INTEGER(intk), INTENT(in) :: ngr, nx, ny, nz
        TYPE(c_ptr) :: dev_fi
        TYPE(c_ptr) :: dev_fo

        ! local variable
        INTEGER :: i,j,k,l,my_device
        REAL(realk), POINTER :: fptr_fi(:,:,:,:)
        REAL(realk), POINTER :: fptr_fo(:,:,:,:)

        ! important
        NULLIFY( fptr_fi )
        NULLIFY( fptr_fo )

        my_device = omp_get_default_device()

        !$omp target device(my_device) is_device_ptr(dev_fi, dev_fo)
        CALL c_f_pointer(dev_fi, fptr_fi, [nx,ny,nz,ngr])
        CALL c_f_pointer(dev_fo, fptr_fo, [nx,ny,nz,ngr])
        !$omp teams distribute parallel do collapse(4)
        DO l = 1, ngr
            DO i = 2, nz-1
                DO j = 2, ny-1
                    DO k = 2, nx-1
                        fptr_fo(k,j,i,l) = 1.0 / 6.0 * &
                            ( fptr_fi(k+1,j,i,l) + fptr_fi(k-1,j,i,l) &
                            + fptr_fi(k,j+1,i,l) + fptr_fi(k,j-1,i,l) &
                            + fptr_fi(k,j,i-1,l) + fptr_fi(k,j,i-1,l) )
                    END DO
                END DO
            END DO
        END DO
        !$omp end teams distribute parallel do
        !$omp end target

    END SUBROUTINE stencil_version_a


    SUBROUTINE stencil_version_b( dev_fi, dev_fo, ngr, nx, ny, nz )
        INTEGER(intk), INTENT(in) :: ngr, nx, ny, nz
        TYPE(c_ptr) :: dev_fi
        TYPE(c_ptr) :: dev_fo

        ! local variable
        INTEGER :: i,j,k,l,my_device
        REAL(realk), POINTER :: fptr_fi(:,:,:,:)
        REAL(realk), POINTER :: fptr_fo(:,:,:,:)

        ! important
        NULLIFY( fptr_fi )
        NULLIFY( fptr_fo )

        my_device = omp_get_default_device()

        !$omp target device(my_device) is_device_ptr(dev_fi, dev_fo)
        CALL c_f_pointer(dev_fi, fptr_fi, [nx,ny,nz,ngr])
        CALL c_f_pointer(dev_fo, fptr_fo, [nx,ny,nz,ngr])
        !$omp teams distribute
        DO l = 1, ngr
            !$omp parallel do collapse(3)
            DO i = 2, nz-1
                DO j = 2, ny-1
                    DO k = 2, nx-1
                        fptr_fo(k,j,i,l) = 1.0 / 6.0 * &
                            ( fptr_fi(k+1,j,i,l) + fptr_fi(k-1,j,i,l) &
                            + fptr_fi(k,j+1,i,l) + fptr_fi(k,j-1,i,l) &
                            + fptr_fi(k,j,i-1,l) + fptr_fi(k,j,i-1,l) )
                    END DO
                END DO
            END DO
            !$omp end parallel do
        END DO
        !$omp end teams distribute
        !$omp end target

        ! Idea: teams = CUDA blocks; threads = CUDA threads
        ! => each block takes care of one grid
        ! => faster than version A of stencil

    END SUBROUTINE stencil_version_b



    SUBROUTINE stencil_version_c( dev_fi, dev_fo, ngr, nx, ny, nz )
        INTEGER(intk), INTENT(in) :: ngr, nx, ny, nz
        TYPE(c_ptr) :: dev_fi
        TYPE(c_ptr) :: dev_fo

        ! local variable
        INTEGER :: i,j,k,l,my_device
        INTEGER :: cnx,cny,cnz,cp,ntot
        REAL(realk), POINTER :: fptr_fi(:)
        REAL(realk), POINTER :: fptr_fo(:)
        INTEGER :: nxyzp_list(4,ngr)

        ! important
        NULLIFY( fptr_fi )
        NULLIFY( fptr_fo )

        ! filling list with grid data (on CPU)
        DO i = 1, ngr
            nxyzp_list(1,i) = nx
            nxyzp_list(2,i) = ny
            nxyzp_list(3,i) = nz
            nxyzp_list(4,i) = (i-1)*nx*ny*nz+1
        END DO

        my_device = omp_get_default_device()

        !$omp target device(my_device) is_device_ptr(dev_fi,dev_fo) map(to:nxyzp_list)
        CALL c_f_pointer(dev_fi, fptr_fi, [nx*ny*nz*ngr])
        CALL c_f_pointer(dev_fo, fptr_fo, [nx*ny*nz*ngr])
        !$omp teams distribute private(cnx,cny,cnz,cp,ntot)
        DO l = 1, ngr
            ! getting grid parameters
            cnx = nxyzp_list(1,l)
            cny = nxyzp_list(2,l)
            cnz = nxyzp_list(3,l)
            cp = nxyzp_list(4,l)
            ntot = cnx * cny * cnz
            ! calling kernel for this "team"
            CALL grid_kernel( fptr_fi(cp:cp+ntot), fptr_fo(cp:cp+ntot), cnx, cny, cnz )
        END DO
        !$omp end teams distribute
        !$omp end target

        ! Idea: teams = CUDA blocks; threads = CUDA threads
        ! => each block takes care of one grid
        ! => faster than version A of stencil

    END SUBROUTINE stencil_version_c


    SUBROUTINE grid_kernel( fptr_fi, fptr_fo, nx, ny, nz )
        !$omp declare target
            INTEGER, INTENT(IN) :: nx, ny, nz
            REAL, TARGET :: fptr_fi(:), fptr_fo(:)
            INTEGER :: i, j, k
            REAL, POINTER :: fptr3d_fi(:,:,:), fptr3d_fo(:,:,:)
            fptr3d_fi(1:nx,1:ny,1:nz) => fptr_fi
            fptr3d_fo(1:nx,1:ny,1:nz) => fptr_fo
            !$omp parallel do collapse(3)
            DO i = 2, nz-1
                DO j = 2, ny-1
                    DO k = 2, nx-1
                        fptr3d_fo(k,j,i) = 1.0 / 6.0 * &
                            ( fptr3d_fi(k+1,j,i) + fptr3d_fi(k-1,j,i) &
                            + fptr3d_fi(k,j+1,i) + fptr3d_fi(k,j-1,i) &
                            + fptr3d_fi(k,j,i-1) + fptr3d_fi(k,j,i-1) )
                    END DO
                END DO
            END DO
            !$omp end parallel do
    END SUBROUTINE



END MODULE gpu_stencil_mod
