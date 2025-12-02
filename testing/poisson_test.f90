Program test_poisson 
    use iso_fortran_env 
    use grid_mod 
    use poisson_mod
    implicit none

    integer :: nx, ny 

    nx = 20
    ny = 20 

    call setup_grid(nx, ny, "null")
    call solve_poisson()

    print *, "phi(10,10) =", phi(10,10)
end program test_poisson