#!/usr/bin/env python3

"""
    Generate the source and target meshes used to test the interpolation code.
"""

import sys
import numpy as np
from scipy.spatial import Voronoi

if len(sys.argv)-1 != 4:
     sys.stderr.write("usage: " + sys.argv[0] + " NPTS NLLOYD SOURCE TARGET\n")
     sys.exit()

MAX_VERTICES = 12               # maximum number of vertices per polygon
NPTS         = int(sys.argv[1]) # number of cells in the mesh
NLLOYD       = int(sys.argv[2]) # number of iterations for Lloyd's algorithm
SOURCE       = sys.argv[3]      # the file where to write the source mesh
TARGET       = sys.argv[4]      # the file where to write the target mesh

def write_mesh(vertices, polygons, filename="output.mesh"):
    """
        Write the mesh in a format similar to the Inria Gamma mesh format

        vertices: a list of lists containing the vertices, in the form [[x0,y0],[x1,y1],...]
        polygons: a list of lists containing the polygons, in the form [[A0,B0,C0,...],[A1,B1,...],...]
            where the values are indices in the table of vertices
        filename: the file to write the mesh to
    """
    global MAX_VERTICES
    with open(filename, 'w') as f:
        # write the vertices
        f.write(f"Vertices\n")
        f.write(f"{len(vertices)}\n")
        for vertex in vertices:
            f.write(f"{vertex[0]} {vertex[1]} 0.0\n")
        # write the polygons
        f.write(f"\nPolygons\n")
        f.write(f"{len(polygons)}\n")
        for polygon in polygons:
            for vtx in polygon:
                f.write(f'{vtx + 1}')
                if vtx != polygon[-1]:
                    f.write(f' ')
            assert(len(polygon) <= MAX_VERTICES)
            # pad with 0 if the polygon has less than MAX_VERTICES vertices
            if (len(polygon) < MAX_VERTICES):
                f.write(f' ')
                for i in range(len(polygon), MAX_VERTICES):
                    f.write(f'{0}')
                    if i < MAX_VERTICES-1:
                        f.write(f' ')
            f.write(f'\n')
            
def append_solution_to_mesh(solution, filename="output.mesh"):
    """
        Write the solution at the end of a file that already contains a mesh

        solution: a list of scalars corresponding to the solution on each cell, in the form [s0,s1,...]
        filename: the file containing the mesh, to which the solution should be written
    """
    global NPTS
    with open(filename, 'a') as f:
        f.write(f"\nSolution\n")
        f.write(f"{NPTS}\n")
        for s in solution:
            f.write(f'{s}\n')

def polygon_area(polygon, vertices):
    """
        Returns the area of the polygon using the trapezoid formula

        polygon : an array containing the indices of the vertices defining the polygon
        vertices: a list of lists containing the vertices, in the form [[x0,y0],[x1,y1],...]
    """
    n = len(polygon)
    area = 0
    for i in range(n):
        p1, p2 = polygon[i], polygon[(i+1)%n]
        x1, y1 = vertices[p1]
        x2, y2 = vertices[p2]
        area += (x2 + x1)*(y2 - y1)
    area /= 2.
    return area

def polygon_GC(polygon, vertices):
    """
        Returns the center of gravity of the polygon

        polygon : an array containing the indices of the vertices defining the polygon
        vertices: a list of lists containing the vertices, in the form [[x0,y0],[x1,y1],...]
    """
    n = len(polygon)
    x, y = 0., 0.
    for i in range(n):
        p1, p2 = polygon[i], polygon[(i+1)%n]
        x1, y1 = vertices[p1]
        x2, y2 = vertices[p2]
        x += (x2**2 + x1*x2 + x1**2)*(y2 - y1)
        y += (y2**2 + y1*y2 + y1**2)*(x1 - x2)
    area = polygon_area(polygon, vertices)
    x /= 6.*area
    y /= 6.*area
    return [x, y]

def symmetrize(points, box):
    """
        Returns a numpy array containing the input points as well as their symmetric
        with respect to each side of the box

        points: input points in a numpy array of shape (n,2)
        box   : an array [xmin, xmax, ymin, ymax] defining the box
    """
    xmin, xmax, ymin, ymax = box
    x, y = points.T[0], points.T[1]

    symmetry_xmin = np.array([xmin - x, y]).T
    symmetry_xmax = np.array([2*xmax - x, y]).T
    symmetry_ymin = np.array([x, ymin - y]).T
    symmetry_ymax = np.array([x, 2*ymax - y]).T
    return np.concatenate((points, symmetry_xmin, symmetry_xmax, symmetry_ymin, symmetry_ymax))


def generate_mesh(random_seed, mesh_name, generate_solution=False):
    """
        Generates a polygonal mesh using Voronoï diagrams and Lloyd's algorithm

        random_seed      : seed to initialize the RNG to get initial random Voronoï generators
        mesh_name        : name of the file to write the mesh to
        generate_solution: append a solution field at the end of the mesh file
    """
    global NPTS, NLLOYD
    global np, Voronoi
    np.random.seed(random_seed)
    bounding_box = [0,1,0,1]              # bounds of the square mesh
    xmin, xmax, ymin, ymax = bounding_box
    points = np.random.rand(NPTS, 2)      # Voronoi generators
    
    # get the symmetric of the generators to enforce that the boundaries belong to the constructed diagram
    points = symmetrize(points, bounding_box)
    
    # Lloyd's algorithm
    for i in range(NLLOYD):
        vor = Voronoi(points)
        vertices = vor.vertices
        polygons = [region for region in vor.regions if len(region) > 0 and -1 not in region]
        new_points = []
        for polygon in polygons:
            GC = polygon_GC(polygon, vertices)
            if (GC[0] > xmin and GC[0] < xmax and GC[1] > ymin and GC[1] < ymax):
                new_points.append(GC)
        assert(len(new_points) == NPTS)
        points = np.array(new_points)
        points = symmetrize(points, bounding_box)
    
    # construct the final mesh with the relaxed generators
    vor = Voronoi(points)
    all_polygons = [region for region in vor.regions if len(region) > 0 and -1 not in region]
    polygons = []
    gcs      = []
    vertices = vor.vertices
    for polygon in all_polygons:
        if polygon_area(polygon, vertices) < 0.:
            polygon.reverse()
        GC = polygon_GC(polygon, vertices)
        if (GC[0] > xmin and GC[0] < xmax and GC[1] > ymin and GC[1] < ymax):
            polygons.append(polygon)
            gcs.append(GC)
    write_mesh(vertices, polygons, mesh_name)

    # append a solution which is used to test the interpolation software
    if generate_solution:
        solution = []
        for gc in gcs:
            solution.append(np.sin(2.*np.pi*gc[0]) + np.cos(2.*np.pi*gc[1]))
        append_solution_to_mesh(solution, mesh_name)


generate_mesh(0, SOURCE, generate_solution=True) # generate the source mesh and its associated solution
generate_mesh(1, TARGET)                         # generate the target mesh onto which the interpolation will be performed
