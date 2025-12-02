Module Poisson_mod
  
    !> ------------------------------------------------------------
    !>  Poisson_mod:
    !>    - Solves Poissons's equation ∇²φ = ρ on a 2D grid
    !>      using Gauss-Seidel iteration with Dirichlet boundaries.
    !>    - The potential φ is stored with ghost cells (0..Nx+1,0..Ny+1)
    !>      and these ghost cells are kept at zero (fixed boundary).
    !>    - Convergence is based on etot / drms <= tol, where
    !>        etot = sum |L[φ] - ρ|
    !>        drms = sqrt(average of (L[φ])²)
    !> ------------------------------------------------------------

    use iso_fortran_env 
    use grid_mod, only : phi, rho, dx, dy, nx_glob, ny_glob
    implicit none 
    private

    public :: solve_poisson 

contains

    !> ------------------------------------------------------------
    subroutine solve_poisson(tol, max_iter, verbose)

        !> ------------------------------------------------------------
        !> Optional paramters:
        !>   - tol.      : Convergence tolerance (default 1.0e-5)
        !>   - max_iter  : Maximum number of iterations (default 20000)
        !>   - verbose   : if .true., print convergence info (default .true.)
        !> ------------------------------------------------------------
        real(real64), intent(in), optional :: tol
        integer, intent(in), optional :: max_iter
        logical, intent(in), optional :: verbose

        real(real64) :: tol_loc
        integer :: max_iter_loc
        logical :: verbose_loc

        integer :: i, j, iter 
        real(real64) :: idx2, idy2, denom 
        real(real64) :: lap, res 
        real(real64) :: etot, drms, drms_sum
        integer :: npoints

        ! Set local parameters from optional arguments
        tol_loc = 1.0e-5_real64
        max_iter_loc = 20000
        verbose_loc = .true.

        if (present(tol)) tol_loc = tol
        if (present(max_iter)) max_iter_loc = max_iter
        if (present(verbose)) verbose_loc = verbose

        ! Precompute constants
        idx2 = 1.0_real64 / (dx * dx)
        idy2 = 1.0_real64 / (dy * dy)
        denom = -2.0_real64 * (idx2 + idy2)

        npoints = (nx_glob) * (ny_glob)

        if (verbose_loc) then
            print *, "-----------------------------------------"
            print *, " Poisson solver: Gauss-Seidel"
            print *, " nx, ny     = ", nx_glob, ny_glob
            print *, " tol        = ", tol_loc
            print *, " max_iter   = ", max_iter_loc
            print *, "-----------------------------------------"
        end if

        ! Main Gauss-Seidel iteration loop
        do iter = 1, max_iter_loc

            ! One sweep over internal points (ghost cells fixed at 0)
            do j = 1, ny_glob
                do i = 1, nx_glob
                    phi(i,j) = (  rho(i,j)                                     &
                                - (phi(i+1,j) + phi(i-1,j)) * idx2             &
                                - (phi(i,j+1) + phi(i,j-1)) * idy2 ) / denom
                end do
            end do

            ! Enforce boundary conditions explicitly (ghost cells = 0)
            phi(0,:) = 0.0_real64
            phi(nx_glob+1,:) = 0.0_real64
            phi(:,0) = 0.0_real64
            phi(:,ny_glob+1) = 0.0_real64

            ! Compute error measures: etot and drms
            etot = 0.0_real64
            drms_sum = 0.0_real64

            do j = 1, ny_glob
                do i =1, nx_glob 
                    lap = (phi(i-1, j) -2.0_real64*phi(i,j) + phi(i+1,j)) * idx2  &
                        + (phi(i,j-1)  -2.0_real64*phi(i,j) + phi(i,j+1)) * idy2

                    res = lap - rho(i,j)
                    etot = etot + abs(res)
                    drms_sum = drms_sum + lap*lap 
                end do
            end do

            if (npoints > 0) then 
                drms = sqrt(drms_sum / real(npoints, real64))
            else
                drms = 0.0_real64
            end if

            ! Avoid division by zero if drms==0 and rtot==0, we are done.
            if (drms ==0.0_real64) then 
                if (etot == 0.0_real64) then 
                    if (verbose_loc) then 
                        print *, "converged (zero residuals) at iteration ", iter
                    end if
                    return 
                else
                    ! drms=0 but etot > 0, continue iterating
                    cycle
                end if
            end if

            if (verbose_loc .and. mod(iter, 100) == 0) then 
                print '(A,I6,2ES14.5)', " iter=", iter, etot, etot/drms
            end if

            ! Convergence test
            if (etot / drms <= tol_loc) then
                if (verbose_loc) then
                    print *, "Poisson solver converged at iter ", iter
                    print *, "Final etot      = ", etot
                    print *, "Final drms      = ", drms
                    print *, "Final etot/drms = ", etot/drms
                end if
                return
            end if

        end do
        
        ! If we reach here, we did not converge within max_iter
        if (verbose_loc) then
            print *, "WARNING: Poisson solver reached max_iter without converging."
            print *, "Final etot/drms = ", etot/drms
        end if

    end subroutine solve_poisson
    !> ------------------------------------------------------------

end module Poisson_mod