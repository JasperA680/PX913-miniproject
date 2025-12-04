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
    extent = [-1.0, 1.0, -1.0, 1.0]
    plt.imshow(
        Ex,
        origin="lower",
        extent=extent,
        aspect="equal",
    )
    plt.colorbar(label="Ex field")
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Electric Field Ex(x,y)")

    # Particle trajectory: y vs x
    plt.figure()
    plt.plot(posX, posY, marker="o", markersize=2, linewidth=1)
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Particle Trajectory: y vs x")
    plt.xlim([-1.0, 1.0])
    plt.ylim([-1.0, 1.0])

    # mark start and end points
    plt.scatter(posX[0], posY[0], s =40, c='green', label='Start')
    plt.scatter(posX[-1], posY[-1], s =40, c='red', label='End')
    plt.legend()
    plt.show()

if __name__ == "__main__":
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    else:
        fname = "data/output_single.nc"
    main(fname)

