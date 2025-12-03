PROGRAM test_particle

  USE ISO_FORTRAN_ENV
  USE particle_mod
  IMPLICIT NONE

  TYPE(particle) :: part
  INTEGER, PARAMETER :: nX = 10, nY = 10
  REAL(REAL64) :: dx, dy, idx, idy
  REAL(REAL64), DIMENSION(nX, nY) :: eX, eY
  CHARACTER(LEN=*), PARAMETER :: mode = "null"

  dx = 2.0_REAL64 / REAL(nX, REAL64)
  dy = 2.0_REAL64 / REAL(nY, REAL64)

  idx = 1.0_REAL64 / dx
  idy = 1.0_REAL64 / dy

  eX = 0.0_REAL64
  eY = 0.0_REAL64

  CALL part%init(mode)

  CALL part%runVV(idx, idy, eX, eY)

  PRINT *, "First 10 x-positions:", part%histPosX(1:10)
  PRINT *, "First 10 y-positions:", part%histPosY(1:10)
  PRINT *
  PRINT *, "First 10 x-velocities:", part%histVelX(1:10)
  PRINT *, "First 10 y-velocities:", part%histVelY(1:10)
  PRINT *
  PRINT *, "First 10 x-accelerations:", part%histAccX(1:10)
  PRINT *, "First 10 y-accelerations:", part%histAccY(1:10)
  PRINT *
  PRINT *, "Iteration on which particle left domain:", part%finalTime


END PROGRAM test_particle
