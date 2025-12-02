Program test_grid 
    use ISO_FORTRAN_ENV
    use grid_mod
    implicit none

    integer :: nx, ny

    ! simple test size 
    nx = 4
    ny = 4

    call setup_grid(nx, ny, "single")

    print *, "dx =", dx, " dy =", dy
    print *, "rho(2,2) =", rho(2,2)
    print *, "phi has shape: ", lbound(phi,1), ubound(phi,1), &
                                lbound(phi,2), ubound(phi,2)

end program test_grid