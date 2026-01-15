# PX913-miniproject
PX913 Scientific Software Development Mini-Project

This mini-project solves Poisson's equation on a two dimensional domain for a prescribed charge density, computes the resulting electric field, and propogates a charged particle through the field using a velocity Verlet integrator.

Results are written to a NetCDF file and visualised using Python.

## Repository Structure


PX913-miniproject/
├── src/                # Fortran source files
│   ├── main.f90
│   ├── grid_mod.f90
│   ├── poisson_mod.f90
│   ├── field_mod.f90
│   ├── particle_mod.f90
│   ├── io_netcdf_mod.f90
│   └── command_line.f90
├── scripts/
│   └── run_project.sh  # Compile + run script
├── python/
│   ├── plot_results.py        # Static plots + MP4 animation
│   └── trajectory_pygame.py   # Interactive Pygame animation
├── data/               # NetCDF output files
├── README.md
└── .venv/              # (optional) Python virtual environment

## Build and run
Use `scripts/run_project.sh` to compile, run, and plot the results.

## Plan 

- Initialise programme using `grid_mod.f90`
- Write `poisson_mod.f90` (Jasper)
- Write `particle_mod.f90` and velocity Verlet (Anton)
- Write `io_netcdf_mod.f90` (Anton)
- Write main driver program and connect modules (Jasper)
- Write build script (Jasper)
- Write python netcdf input and visualiser (Jasper)
- Add time-dependence into the trajectory 
