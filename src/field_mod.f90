module field_mod 
    !> ------------------------------------------------------------------
    !>  field_mod:
    !>     - Computes electric field components Ex, Ey from the
    !>       potential phi using central finite differences.
    !>     - Phi is defined on 0...Nx+1, 0...Ny+1 grid with ghost cells.
    !>     - Ex, Ey are defined on the real grid 1...Nx, 1...Ny.
    !> ------------------------------------------------------------------

    use iso_fortran_env 
    use grid_mod, only : phi, dx, dy, nx_glob, ny_glob
    implicit none 
    private

    ! Electric field arrays, availible to other modules
    ! ANTON YOU MIGHT WANT TO CHANGE THIS FOR PARTICLE_MOD !!!!!
    real(real64), allocatable, public :: Ex(:,:), Ey(:,:)

    public :: compute_fields

contains 

    !> --------------------------------------------------------------------
    subroutine compute_fields()
        ! Assumes phi has already been computed by the Poisson solver

        integer :: i,j
        real(real64) :: inv2dx, inv2dy

        if (.not. allocated(Ex)) allocate(Ex(1:nx_glob, 1:ny_glob))
        if (.not. allocated(Ey)) allocate(Ey(1:nx_glob, 1:ny_glob))

        inv2dx = 1.0_real64 / (2.0_real64*dx)
        inv2dy = 1.0_real64 / (2.0_real64*dy)

        do j =1, ny_glob
            do i=1, nx_glob 
                Ex(i,j) = - (phi(i+1,j) - phi(i-1,j)) * inv2dx
                Ey(i,j) = - (phi(i,j+1) - phi(i,j-1)) * inv2dy
            end do
        end do

    end subroutine compute_fields
    !> --------------------------------------------------------------------

end module field_mod
