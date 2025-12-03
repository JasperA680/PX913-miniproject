import sys 
from netCDF4 import Dataset
import numpy as np
import matplotlib.pyplot as plt

def main(filename: str) -> None:
    print(f"Opening NetCDF file: {filename}")
    ds = Dataset(filename, mode="r")

    # Read Global attributes/axes
    Nx = int(getattr(ds, 'Nx'))
    Ny = int(getattr(ds, 'Ny'))

    x = np.array(getattr(ds, "x"))
    y = np.array(getattr(ds, "y"))
    t = np.array(getattr(ds, "t"))

    # Read field variables (note visulisation only for Ex field here)
    Ex = ds.variables["Ex"][:]

    # Read particle kinematics
    posX = ds.variables["posX"][:]
    posY = ds.variables["posY"][:]

    ds.close()

    # Pseudocolour plot of Ex(x,y)
    plt.figure()
    
