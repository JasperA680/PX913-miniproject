# PX913-miniproject
PX913 Scientific Software Development Mini-Project

This repository contains the full solution for the PX913 Scientific SoftwareDevelopment mini-project.

The project solves Poisson's equation using Gauss-Seidel iteration on a 2D grid, computes the electric field,
simulates a charged particle using velocity Verlet, and writes all data to a NetCDF file.

## Directory Structure

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
