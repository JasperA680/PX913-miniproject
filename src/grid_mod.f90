module grid_mod 
    !> -------------------------------------------------------------
    !>   grid_mod:
    !>     - Defines the physical domain [-1,1]x[-1,1].
    !>     - Allocates rho(i,j) on the real grid 1...Nx, 1...Ny.
    !>     - Allocates phi(i,j) on an extended grid with ghost cells
    !>       0...Nx+1, 0...Ny+1.
    !>     - Computes dx, dy based on Nx, Ny.
    !>     - Fills rho according to the chosen problem:
    !>          "null", "single", or "double" Gaussian.
    !> -------------------------------------------------------------


end module grid_mod 