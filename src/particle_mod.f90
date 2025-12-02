!> --------------
!> Module housing particle object and associated methods
!> -
!> -
!> -
!> -
!> -
!> -
!> -
!> -
!> -
!> -
!> -
!> -
!> --------------

MODULE particle_mod
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE
  SAVE

  INTEGER, PARAMETER :: numIters = 1000
  INTEGER, PARAMETER :: q = -1
  REAL(REAL64), PARAMETER :: dt = 0.01_REAL64

  TYPE particle
    !> Current position, velocity, acceleration of the particle
    REAL(REAL64), DIMENSION(2) :: pos, vel, acc

    !> Histories of the x- and y- positions, velocities, accelerations
    REAL(REAL64), DIMENSION(:), ALLOCATABLE :: histPosX, histPosY, &
                           histVelX, histVelY, histAccX, histAccY

    !> Electric fields to calculate acceleration
    REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: electricX, electricY

    CONTAINS

    PROCEDURE :: init => particleInit

  END TYPE particle

  CONTAINS

  !> Initialises particle position and velocity, allocates history arrays,
  !> and copies electric field arrays
  SUBROUTINE particleInit(this, mode, eX, eY)
    CLASS(particle), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: mode
    REAL(REAL64), DIMENSION(0:, 0:), INTENT(IN) :: eX, eY ! Arrays for electric field components

    ALLOCATE(this%histPosX(0:numIters))
    ALLOCATE(this%histPosY(0:numIters))
    ALLOCATE(this%histVelX(0:numIters))
    ALLOCATE(this%histVelY(0:numIters))
    ALLOCATE(this%histAccX(0:numIters))
    ALLOCATE(this%histAccY(0:numIters))

    !> Sets the initial conditions
    SELECT CASE (TRIM(mode))
      CASE ("null")
        this%pos = [0.0_REAL64, 0.0_REAL64]
        this%vel = [0.1_REAL64, 0.1_REAL64]
      CASE ("single")
        this%pos = [0.1_REAL64, 0.0_REAL64]
        this%vel = [0.0_REAL64, 0.0_REAL64]
      CASE ("double")
        this%pos = [0.0_REAL64, 0.5_REAL64]
        this%vel = [0.0_REAL64, 0.0_REAL64]
    END SELECT

    this%histPosX(0) = this%pos(1)
    this%histPosY(0) = this%pos(2)
    this%histVelX(0) = this%vel(1)
    this%histVelY(0) = this%vel(2)

    this%electricX = eX
    this%electricY = eY

  END SUBROUTINE particleInit



END MODULE particle_mod






