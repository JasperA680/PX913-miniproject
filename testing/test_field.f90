Program test_field 
    use iso_fortran_env
    use grid_mod
    use poisson_mod
    use field_mod
    implicit none

    integer :: nx, ny

    nx = 20
    ny = 20

    call setup_grid(nx, ny, "single")
    call solve_poisson()
    call compute_fields()

    print *, "Ex(10,10) =", Ex(10,10)
    print *, "Ey(10,10) =", Ey(10,10)
end program test_field