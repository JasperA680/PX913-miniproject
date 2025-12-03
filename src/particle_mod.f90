!> --------------
!> Module housing particle object and associated methods
!>   - defines the particle derived type, which consists of the particle's
!>     current position, velocity, and acceleration, and also contains
!>     histories of each too. Also stores the final iteration number
!>     up to which the particle is inside of the domain
!>   - houses an initialiser which allocates all the necessary arrays,
!>     and also inputs the initial condition
!>   - contains the implementation of the velocity Verlet algorithm
!>   - has small helper methods, namely for obtaining grid coordinates
!>     and for storing the current kinematics in the history arrays
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

    !> Integer storing the final iteration calculated
    INTEGER :: finalTime

    CONTAINS

    PROCEDURE :: init => particleInit
    PROCEDURE :: getGC => particleGetGridCell
    PROCEDURE :: runVV => particleVelocityVerlet
    PROCEDURE :: store => particleStoreHistory

  END TYPE particle

  CONTAINS

  !> Initialises particle position and velocity, allocates history arrays,
  !> and copies electric field arrays
  SUBROUTINE particleInit(this, mode)
    CLASS(particle), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: mode

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
      CASE DEFAULT
        PRINT *, "Input string not one of required values"
        STOP 1
    END SELECT

  END SUBROUTINE particleInit


  !> Subroutine to store current kinematics in history arrays
  SUBROUTINE particleStoreHistory(this, iter)
    CLASS(particle), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iter        ! Assumes this is within the bounds of the history arrays

    this%histPosX(iter) = this%pos(1)
    this%histPosY(iter) = this%pos(2)
    this%histVelX(iter) = this%vel(1)
    this%histVelY(iter) = this%vel(2)
    this%histAccX(iter) = this%acc(1)
    this%histAccY(iter) = this%acc(2)

  END SUBROUTINE particleStoreHistory


  !> Function to return grid cell which the particle is in, depending on
  !> its position in the domain
  FUNCTION particleGetGridCell(this, one_o_dx, one_o_dy)
    INTEGER, DIMENSION(2) :: particleGetGridCell
    CLASS(particle), INTENT(IN) :: this
    REAL(REAL64), INTENT(IN) :: one_o_dx, one_o_dy

    particleGetGridCell(1) = FLOOR((this%pos(1) - 1.0_REAL64)*one_o_dx) + 1
    particleGetGridCell(2) = FLOOR((this%pos(2) - 1.0_REAL64)*one_o_dy) + 1

  END FUNCTION particleGetGridCell


  !> Performs velocity Verlet algorithm and stores kinematics components
  !> in each of the particle arrays
  SUBROUTINE particleVelocityVerlet(this, one_o_dx, one_o_dy, eX, eY)
    CLASS(particle), INTENT(INOUT) :: this
    REAL(REAL64), INTENT(IN) :: one_o_dx, one_o_dy    ! Inverses of grid spacings
    REAL(REAL64), DIMENSION(1:, 1:), INTENT(IN) :: eX, eY   ! Electric field arrays

    INTEGER, DIMENSION(2) :: gridCoords    ! Grid cell which the particle is in
    REAL(REAL64), DIMENSION(2) :: prevAcc
    INTEGER :: i

    gridCoords = this%getGC(one_o_dx, one_o_dy)
    this%acc(1) = q * eX(gridCoords(1), gridCoords(2))
    this%acc(2) = q * eY(gridCoords(1), gridCoords(2))

    CALL this%store(0)

    this%finalTime = numIters

    DO i = 1, numIters
      
      !> Compute velocity Verlet algorithm

      this%pos = this%pos + this%vel * dt + 0.5 * this%acc * dt * dt

      prevAcc = this%acc
      gridCoords = this%getGC(one_o_dx, one_o_dy)
      this%acc(1) = q * eX(gridCoords(1), gridCoords(2))
      this%acc(2) = q * eY(gridCoords(1), gridCoords(2))

      this%vel = this%vel + 0.5 * dt * (prevAcc + this%acc)

      CALL this%store(i)

      !> Checking to see if particle has left the domain
      IF(.NOT.(this%pos(1) .LE. 1.0_REAL64 .AND. this%pos(1) .GE. -1.0_REAL64 .AND. &
               this%pos(2) .LE. 1.0_REAL64 .AND. this%pos(2) .GE. -1.0_REAL64)) THEN

        PRINT *, "Particle has left the domain"
        this%finalTime = i

        !> Populating the rest of the history arrays with the current kinematics and exiting
        !> Also copies into the current iteration to avoid an empty slice (e.g. the particle
        !> leaves the domain on the final iteration)
        this%histPosX(i:numIters) = this%pos(1)
        this%histPosY(i:numIters) = this%pos(2)
        this%histVelX(i:numIters) = this%vel(1)
        this%histVelY(i:numIters) = this%vel(2)
        this%histAccX(i:numIters) = this%acc(1)
        this%histAccY(i:numIters) = this%acc(2)

        EXIT

      END IF

    END DO

  END SUBROUTINE particleVelocityVerlet

END MODULE particle_mod






