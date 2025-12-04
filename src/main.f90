program main 
    !> ----------------------------------------------------------------------
    !>  Main driver for PX913 mini-project
    !>  
    !>  Flow:
    !>    1.  Parse command line arguments using command_line module
    !>    2.  Setup grid using grid_mod                         [grid_mod]
    !>    3.  Solve Poisson equation using poisson_mod          [poisson_mod]
    !>    4.  Compute electric fields using field_mod           [field_mod]
    !>    5.  Initialize and run particles using particle_mod   [particle_mod]
    !>    6.  (To do) Add Antons NetCDF output module           [io_netcdf_mod]
    !> ----------------------------------------------------------------------

    use iso_fortran_env, only : dp => real64, i32 => int32
    use command_line, only : parse_args, get_arg 
    use grid_mod, only : setup_grid, dx, dy, rho, phi
    use poisson_mod, only : solve_poisson
    use field_mod, only : compute_fields, Ex, Ey
    use particle_mod, only : particle
    USE io_netcdf_mod
    implicit none

    ! parsed arguments
    integer(i32) :: nx_i32, ny_i32
    integer :: nx, ny
    character(len=30) :: problem_str
    character(len=32) :: problem 

    logical :: ok_nx, ok_ny, ok_prob 

    ! NetCDF error flag
    INTEGER :: ierr

    ! particle related
    type(particle) :: p 
    real(dp) :: one_o_dx, one_o_dy

    ! ---------------------------------------------
    ! 1. Parse command line arguments
    ! ---------------------------------------------
    call parse_args()

    ok_nx = get_arg("nx", nx_i32)
    ok_ny = get_arg("ny", ny_i32)
    ok_prob = get_arg("problem", problem_str)

    if(.not.(ok_nx .and. ok_ny .and. ok_prob)) then
        print *, "Error: Missing required command line arguments."
        print *, "  Required arguments:"
        print *, "    --nx       : Number of grid points in x"
        print *, "    --ny       : Number of grid points in y"
        print *, "    --problem  : Problem type (e.g., 'single', 'double')"
        stop 1
    end if

    nx = nx_i32
    ny = ny_i32
    problem = trim(problem_str)

    
    print *, "------------------------------------------------------------"
    print *, "  nx       = ", nx
    print *, "  ny       = ", ny
    print *, "  problem  = ", trim(problem)
    print *, "------------------------------------------------------------"

    ! ---------------------------------------------
    ! 2. Setup grid
    ! ---------------------------------------------
    call setup_grid(nx, ny, problem)
    print *, "Grid and rho initialised."

    ! ---------------------------------------------
    ! 3. Solve Poisson equation for phi
    ! ---------------------------------------------
    call solve_poisson()
    print *, "Poisson equation solved."

    ! ---------------------------------------------
    ! 4. Compute electric fields Ex, Ey
    ! ---------------------------------------------
    call compute_fields()
    print *, "Electric fields computed."

    ! ---------------------------------------------
    ! 5. Initialize and run particles with V elocity Verlet
    ! ---------------------------------------------
    one_o_dx = 1.0_dp / dx
    one_o_dy = 1.0_dp / dy

    call p%init(problem)
    call p%runVV(one_o_dx, one_o_dy, Ex, Ey)

    print *, "Particle simulation complete."
    print *, "Final time index =", p%finalTime
    print *, "Final position   =", p%pos

    ! ---------------------------------------------
    ! (To do) 6. Output results using NetCDF
    ! ---------------------------------------------
    
    CALL write_to_netcdf("data/output_single.nc", ierr, rho, phi(1:nx, 1:ny), Ex, Ey, p)
    print *, "Program complete."

end program main
