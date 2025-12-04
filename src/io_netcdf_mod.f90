!> -------------
!> Module to handle writing to NetCDF output
!>   - Requires particle_mod, grid_mod, and domain_tools modules
!>   - Takes in the particle and fields as inputs
!>   - Writes all of the arrays
!>   -
!>   -
!>   -
!>   -
!>   -
!>   -
!>   -
!>   -
!> -------------

MODULE io_netcdf_mod

  USE ISO_FORTRAN_ENV
  USE grid_mod, ONLY: dx, dy, nx_glob, ny_glob
  USE particle_mod
  USE domain_tools
  USE netcdf

  IMPLICIT NONE

  CONTAINS

  !> Subroutine written with help from the Workshop 7 write_netcdf_array.f90 file
  !> which itself was based on the example at
  !> https://people.sc.fsu.edu/~jburkardt/f_src/netcdf/netcdf.html
  !> (web page unfortunately now taken down)
  !>
  !> Much of this code is also taken from my Assignment 3 netcdf writer

  SUBROUTINE write_to_netcdf(fileName, ierr, rho, phi, Ex, Ey, part)

    CHARACTER(LEN=*), INTENT(IN) :: fileName
    INTEGER, INTENT(OUT) :: ierr                ! NetCDF error flag
    REAL(REAL64), DIMENSION(:,:), INTENT(IN) :: rho, phi, Ex, Ey  ! The real parts of the fields (i.e. no ghost cells)
    TYPE(particle), INTENT(IN) :: part

    REAL(REAL64), DIMENSION(:), ALLOCATABLE :: xAxis, yAxis, tAxis
    INTEGER, DIMENSION(3) :: array_ids, array_dims
    CHARACTER(LEN=1), DIMENSION(3) :: array_names = ['x', 'y', 't']

    INTEGER :: i, file_id
    INTEGER, DIMENSION(4) :: field_ids        ! Array of ids for the fields, in the order [rho, phi, Ex, Ey]
    INTEGER, DIMENSION(6) :: kinematic_ids    ! Array of ids for the kinematics, in the order [posX, posY, velX, velY, accX, accY]

    array_dims = [nx_glob, ny_glob, numIters + 1]

    

    !> Populate axes
    CALL create_axis(xAxis, nx_glob, [-1.0_REAL64, 1.0_REAL64])
    CALL create_axis(yAxis, ny_glob, [-1.0_REAL64, 1.0_REAL64])
    ALLOCATE(tAxis(0:numIters))
    DO i = 0, numIters
      tAxis(i) = REAL(i, REAL64) * dt
    END DO

    ierr = nf90_create(fileName, NF90_CLOBBER, file_id)
    !> Error checker, taken from the Workshop 7 example, also copied for all checks below.
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    !> Setting up global parameters
    ierr = nf90_put_att(file_id, NF90_GLOBAL, "Nx", nx_glob)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_att(file_id, NF90_GLOBAL, "Ny", ny_glob)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_att(file_id, NF90_GLOBAL, "dx", dx)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_att(file_id, NF90_GLOBAL, "dy", dy)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_att(file_id, NF90_GLOBAL, "dt", dt)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

        ! Maximum number of iterations in the velocity Verlet algorithm
    ierr = nf90_put_att(file_id, NF90_GLOBAL, "iters_VV_max", numIters)  
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

        ! Simulated number of iterations (until particle leaves the domain)
    ierr = nf90_put_att(file_id, NF90_GLOBAL, "iters_VV_sim", part%finalTime)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    !> Setting up axes
    ierr = nf90_put_att(file_id, NF90_GLOBAL, array_names(1), xAxis)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_att(file_id, NF90_GLOBAL, array_names(2), yAxis)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_att(file_id, NF90_GLOBAL, array_names(3), tAxis)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    !> Defining dimensions
    DO i = 1, 3
      ierr = nf90_def_dim(file_id, array_names(i), array_dims(i), array_ids(i))
      IF(ierr /= NF90_NOERR) THEN
        PRINT *, TRIM(nf90_strerror(ierr))
        RETURN
      END IF
    END DO

    !> Defining field variables
    ierr = nf90_def_var(file_id, "rho", NF90_DOUBLE, array_ids(1:2), field_ids(1))
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_def_var(file_id, "phi", NF90_DOUBLE, array_ids(1:2), field_ids(2))
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_def_var(file_id, "Ex", NF90_DOUBLE, array_ids(1:2), field_ids(3))
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_def_var(file_id, "Ey", NF90_DOUBLE, array_ids(1:2), field_ids(4))
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    !> Defining kinematics variables
    ierr = nf90_def_var(file_id, "posX", NF90_DOUBLE, array_ids(3), kinematic_ids(1))
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_def_var(file_id, "posY", NF90_DOUBLE, array_ids(3), kinematic_ids(2))
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_def_var(file_id, "velX", NF90_DOUBLE, array_ids(3), kinematic_ids(3))
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_def_var(file_id, "velY", NF90_DOUBLE, array_ids(3), kinematic_ids(4))
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_def_var(file_id, "accX", NF90_DOUBLE, array_ids(3), kinematic_ids(5))
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_def_var(file_id, "accY", NF90_DOUBLE, array_ids(3), kinematic_ids(6))
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    !> Ending metadata definition
    ierr = nf90_enddef(file_id)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF
    
    !> Putting variables
    ierr = nf90_put_var(file_id, field_ids(1), rho)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_var(file_id, field_ids(2), phi)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_var(file_id, field_ids(3), Ex)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_var(file_id, field_ids(4), Ey)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_var(file_id, kinematic_ids(1), part%histPosX)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_var(file_id, kinematic_ids(2), part%histPosY)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_var(file_id, kinematic_ids(3), part%histVelX)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_var(file_id, kinematic_ids(4), part%histVelY)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_var(file_id, kinematic_ids(5), part%histAccX)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    ierr = nf90_put_var(file_id, kinematic_ids(6), part%histAccY)
    IF(ierr /= NF90_NOERR) THEN
      PRINT *, TRIM(nf90_strerror(ierr))
      RETURN
    END IF

    !> Closing file and printing result of file write, regardless of whether error occurs
    ierr = nf90_close(file_id)
    PRINT *, "NetCDF: ", TRIM(nf90_strerror(ierr))

  END SUBROUTINE write_to_netcdf

END MODULE io_netcdf_mod
