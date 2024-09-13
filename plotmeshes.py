#!/usr/bin/env python3

"""
    Plot the source and target meshes as well as their associated solution.
"""

import sys
import numpy as np
import matplotlib.pyplot as plt

if len(sys.argv)-1 != 2:
     sys.stderr.write("usage: " + sys.argv[0] + " SOURCE TARGET\n")
     sys.exit()

SOURCE = sys.argv[1] # the file containing the source of the interpolation
TARGET = sys.argv[2] # the file containing the target of the interpolation

def plot(vertices, polygons, solution, ax, colormap="viridis"):
    """
        Plot the input mesh and its associated solution

        vertices: a list of lists containing the vertices, in the form [[x0,y0],[x1,y1],...]
        polygons: a list of lists containing the polygons, in the form [[A0,B0,C0,...],[A1,B1,...],...]
            where the values are indices in the table of vertices
        solution: a list of scalars corresponding to the solution on each cell, in the form [s0,s1,...]
        ax      : the matplotlib.pyplot axis to plot to
        colormap: name of the matplotlib.pyplot colormap to use
    """
    global plt
    norm = plt.Normalize(min(solution), max(solution))
    cmap = plt.cm.get_cmap(colormap)
    colors = cmap(norm(solution))

    for poly, color in zip(polygons, colors):
        x = []
        y = []
        for idx in poly:
            vtx = vertices[idx-1]
            x.append(vtx[0])
            y.append(vtx[1])
        ax.fill(x, y, facecolor=color, edgecolor='black')

    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array(solution)
    plt.colorbar(sm, ax=ax, fraction=0.045, pad=0.04)
    ax.set_aspect('equal')

def read(mesh_file):
    """
        Read a mesh from a file and possibly a solution

        mesh_file: name of the file to read the mesh and solution from
    """
    with open(mesh_file, 'r') as f:
        content = f.read().splitlines()

    i = 0

    # read the vertices
    while not content[i] == "Vertices":
        i = i+1
    i = i+1
    nvertices = int(content[i])
    i0 = i+1
    vertices = []
    for i in range(i0, i0+nvertices):
        vertices.append(content[i].split())

    # read the polygons
    while not content[i] == "Polygons":
        i = i+1
    i = i+1
    npolygons = int(content[i])
    i0 = i+1
    polygons = []
    for i in range(i0, i0+npolygons):
        polygons.append(content[i].split())

    # read the solution
    while i < len(content) and not content[i] == "Solution":
        i = i+1
    if i == len(content): # if no solution is present in the file, construct a dummy one
        solution = [0. for i in range(npolygons)]
    else:                 # else read the solution from the file
        i = i+1
        nsolution = int(content[i])
        i0 = i+1
        solution = []
        for i in range(i0, i0+nsolution):
            solution.append(content[i])
        solution = [float(s) for s in solution]

    vertices = [[float(x), float(y)] for x,y,_ in vertices]
    polygons = [[int(vtx) for vtx in poly if not int(vtx) == 0] for poly in polygons]
    return vertices, polygons, solution


fig, (ax_source, ax_target) = plt.subplots(1,2)
fig.suptitle('Conservative P0 interpolation')

vertices, polygons, solution = read(SOURCE)
plot(vertices, polygons, solution, ax_source)
ax_source.set_title('source')
vertices, polygons, solution = read(TARGET)
plot(vertices, polygons, solution, ax_target)
ax_target.set_title('target')

plt.tight_layout() # to prevent colorbars and plots overlapping
plt.show()
