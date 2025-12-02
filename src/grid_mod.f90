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

    USE ISO_FORTRAN_ENV 
    IMPLICIT NONE
    PRIVATE

    ! Public variables that other modules will need
    real(real64), allocatable, public :: rho(:,:)       ! charge density on real grid
    real(real64), allocatable, public :: phi(:,:)       ! potential on extended grid
    real(real64), public :: dx, dy                      ! grid spacing
    integer, public :: nx_glob, ny_glob                 ! number of grid points in x and y

    public :: setup_grid

    contains

        subroutine setup_grid(nx, ny, problem)

            integer, intent(in) :: nx, ny 
            character(*), intent(in) :: problem 

            integer :: i,j
            real(real64) :: x, y
            real(real64) , parameter :: xmin = -1.0_real64, xmax = 1.0_real64
            real(real64) , parameter :: ymin = -1.0_real64, ymax = 1.0_real64
            
            ! Store for global access
            nx_glob = nx
            ny_glob = ny 

            ! Compute grid spacing
            dx = (xmax - xmin) / real(nx, real64)
            dy = (ymax - ymin) / real(ny, real64)

            ! Allocate arrays:
            !   - rho on real grid 1...nx, 1...ny
            !   - phi with ghost cells: 0...nx+1, 0...ny+1
            allocate(rho(1:nx, 1:ny))
            allocate(phi(0:nx+1, 0:ny+1))

            ! initialise everything to zero
            rho = 0.0_real64
            phi = 0.0_real64

            ! Fill rho according to the chosen problem
            select case (trim(problem))

            case ("null")
                    ! rho is already zero
            
            case ("single")
                ! Single Gaussian at center
                do j = 1, ny
                    y = ymin + (real(j, real64) - 0.5_real64) * dy 
                    do i = 1, nx 
                        x = xmin + (real(i, real64) - 0.5_real64) * dx 
                        rho(i,j) = exp(-(x/0.1_real64)**2 - (y/0.1_real64)**2)
                    end do
                end do
            
            case ("double")
                ! Two Gaussians with different widths and offsets
                do j = 1, ny
                    y = ymin + (real(j, real64) - 0.5_real64) * dy 
                    do i = 1, nx 
                        x = xmin + (real(i, real64) - 0.5_real64) * dx 
                        rho(i,j) = exp(-((x + 0.25_real64)/0.1_real64)**2 - ((y + 0.25_real64)/0.1_real64)**2) &
                                 + exp(-((x - 0.75_real64)/0.2_real64)**2 - ((y - 0.75_real64)/0.2_real64)**2)
                    end do
                end do
            
            case default
                print *, "Error: Unknown problem type = ", trim(problem)
                stop 1
            end select
            
        end subroutine setup_grid

end module grid_mod 