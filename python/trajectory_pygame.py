#================================================
# Note that this code requires pygame to be installed.
# You can install it via pip if you don't have it in your
# Python environment:
#    pip install pygame
#
# To run this script from the command line, use:
#    python trajectory_pygame.py [datafile.nc]
#================================================


import sys
import numpy as np
import pygame
from netCDF4 import Dataset


def load_trajectory(filename: str):
    print(f"Opening NetCDF file: {filename}")
    ds = Dataset(filename, mode="r")

    # Read particle kinematics and time axis
    posX = np.array(ds.variables["posX"][:])
    posY = np.array(ds.variables["posY"][:])
    t = np.array(getattr(ds, "t"))  

    ds.close()
    return posX, posY, t


def world_to_screen(x, y, width, height):
    """
    Map world coords in [-1, 1] x [-1, 1] to screen pixels.
    x_world = -1 -> 0, +1 -> width
    y_world = -1 -> height, +1 -> 0  (invert y so up is +y)
    """
    sx = (x + 1.0) * 0.5 * width
    sy = (1.0 - (y + 1.0) * 0.5) * height
    return int(sx), int(sy)


def run_pygame_animation(posX, posY, t, width=800, height=800, fps=60):
    pygame.init()

    screen = pygame.display.set_mode((width, height))
    pygame.display.set_caption("Particle Trajectory Animation")

    clock = pygame.time.Clock()

    # Colours
    BLACK = (0, 0, 0)
    WHITE = (255, 255, 255)
    RED   = (255, 0, 0)
    GREEN = (0, 255, 0)
    GREY  = (150, 150, 150)

    font = pygame.font.SysFont(None, 24)

    n_steps = len(posX)
    idx = 0   # current frame index
    running = True

    # Precompute screen coordinates for the whole path for efficiency
    path_points = [
        world_to_screen(posX[i], posY[i], width, height)
        for i in range(n_steps)
    ]

    while running:
        # --- Event handling ---
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        # --- Update frame index ---
        if idx < n_steps - 1:
            idx += 1  

        # --- Drawing ---
        screen.fill(BLACK)

        # Draw a faint box for the domain [-1,1] x [-1,1]
        pygame.draw.rect(
            screen,
            GREY,
            pygame.Rect(0, 0, width, height),
            width=2
        )

        # Draw the path up to the current index
        if idx > 1:
            pygame.draw.lines(
                screen,
                WHITE,
                False,
                path_points[: idx + 1],
                2
            )

        # Draw start point
        pygame.draw.circle(
            screen,
            GREEN,
            path_points[0],
            6
        )

        # Draw current particle position
        pygame.draw.circle(
            screen,
            RED,
            path_points[idx],
            6
        )

        # Draw time text
        time_text = font.render(f"t = {t[idx]:.3f}", True, WHITE)
        screen.blit(time_text, (10, 10))

        # Flip the display
        pygame.display.flip()

        # Cap the frame rate
        clock.tick(fps)

    pygame.quit()


def main():
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    else:
        fname = "data/output_single.nc"

    posX, posY, t = load_trajectory(fname)
    run_pygame_animation(posX, posY, t)


if __name__ == "__main__":
    main()