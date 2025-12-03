#!/usr/bin/env bash

# =============================================================
# PX913 Miniproject run script
#
# Note: run chmod +x scripts/run_project.sh to make this script executable
#
# This script:
#     1. Compiles the Fortran source code into mini_project.ex 
#     2. Runs it with nx=100, ny=100, problem=single 
# ============================================================= 

set -e  # Exit on error

# 1. Compile the Fortran source code

FC=gfortran
FFlags="-std=f2008 -O2"

echo "Compiling source code..."
$FC $FFlags \
    src/command_line.f90 \
    src/grid_mod.f90 \
    src/poisson_mod.f90 \
    src/field_mod.f90 \
    src/particle_mod.f90 \
    src/main.f90 \
    -o mini_project.exe

echo "Compilation complete."

# 2. Run the executable with specified parameters

NX=100
NY=100
PROBLEM="single"

echo "Running mini_project.exe with nx=${NX}, ny=${NY}, problem=${PROBLEM}..."
./mini_project.exe nx=${NX} ny=${NY} problem=${PROBLEM}

echo "Run complete."
