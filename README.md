# PX913-miniproject
PX913 Scientific Software Development Mini-Project

This mini-project solves Poisson's equation on a two dimensional domain for a prescribed charge density, computes the resulting electric field, and propogates a charged particle through the field using a velocity Verlet integrator.

Results are written to a NetCDF file and visualised using Python.

## Directory Structure

## Repository Structure

```text
PX913-miniproject/
├── data/
│   └── output_single.nc          # NetCDF output file
├── python/
│   ├── plot_results.py           # Static plots + MP4 animation
│   └── trajectory_pygame.py      # Interactive Pygame trajectory visualisation
├── scripts/
│   └── run_project.sh            # Compile + run script
├── src/
│   ├── command_line.f90          # Command-line argument parser
│   ├── create_axis.f90           # Utility for generating coordinate axes
│   ├── field_mod.f90             # Electric field computation
│   ├── grid_mod.f90              # Grid setup and charge density
│   ├── io_netcdf_mod.f90         # NetCDF output writer
│   ├── main.f90                  # Main driver program
│   ├── particle_mod.f90          # Particle object and velocity-Verlet integrator
│   └── poisson_mod.f90           # Poisson solver
├── testing/
│   ├── particle_test.f90         # Particle module test
│   ├── poisson_test.f90          # Poisson solver test
│   ├── test_field.f90            # Field module test
│   └── test_grid.f90             # Grid module test
├── .gitignore
└── README.md
```

## Physical Model

- Domain: square $[-1,1] \times [-1,1]$
- Poisson equation:
  $$
  \nabla^2 \phi = \rho(x,y)
  $$
- Boundary conditions: Dirichlet ($\phi = 0$ on all boundaries)
- Charge density options:
  - `null`   – zero charge density
  - `single` – single Gaussian charge
  - `double` – two Gaussian charges
- Electric field:
  $$
  \mathbf{E} = -\nabla \phi
  $$
- Particle dynamics:
  - Charged particle evolved using velocity-Verlet
  - Time step and number of iterations fixed in `particle_mod`

---

## Requirements

### Fortran
- `gfortran` (Fortran 2008 compatible)

### NetCDF
- netCDF-Fortran library  
Check availability with:
```bash
nf-config --version