program test_particle
    Use iso_fortran_env, only : dp => real64
    Use grid_mod
    Use poisson_mod
    Use field_mod
    Use particle_mod
    implicit none

    integer :: nx, ny 
    type (particle) :: p
    real(dp) :: one_o_dx, one_o_dy

    nx = 20
    ny = 20

    call setup_grid(nx, ny, "single")
    call solve_poisson(verbose=.false.)
    call compute_fields()

    one_o_dx = 1.0_dp / dx 
    one_o_dy = 1.0_dp / dy

    call p%init("single")
    call p%runVV(one_o_dx, one_o_dy, Ex, Ey)

    print *, "Final time index =", p%finalTime
    print *, "Final position   =", p%pos
    print *, "histPosX(0), histPosY(0) =", p%histPosX(0), p%histPosY(0)
    print *, "histPosX(10), histPosY(10) =", p%histPosX(10), p%histPosY(10)
end program test_particle
