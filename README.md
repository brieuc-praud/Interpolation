# $P0$ Conservative Interpolation
A Fortran code to perform $P0$ conservative interpolation of a scalar field from one mesh to another.

## Usage
The provided Makefile allows to run a demo of the code by running the following command:
```console
    make run
```
This demo does three main steps
- It generates a source mesh with an associated solution and a target mesh with no associated solution by calling a Python script.
- It performs the interpolation by calling the main executable. This appends the interpolated solution at the end of the target mesh file.
- It plots the source mesh with the original solution and the target mesh with the interpolated solution by calling the relevant Python script.

## Overview

### Conservative interpolation
The conservation property arises from the fact that the field value in each cell of the target mesh is obtained as an average,
weighted by the area of the intersections with the cells from the source mesh
(see Farrell, P. E., & Maddison, J. R. "Conservative interpolation between volume meshes by local Galerkin projection" (2011) for details).
This same paper explains how a linear time complexity is achieved using an advancing front algorithm while the naive approach has quadratic complexity.

### Intersection area
The conservative interpolation relies on the computation of the area of the overlap between each cell of both meshes.
This computation is performed in two steps for each cell:
- The Sutherland-Hodgman algorithm is used to clip a cell from one mesh with respect to a cell from the other mesh.
This gives the intersection of the two polygons.
- The area of the intersection polygon is computed using the trapezoid formula.

## Debug

### Compile in debug mode
The code can be compiled in debug mode by setting the `DEBUG` variable to 1: `make DEBUG=1` or `make run DEBUG=1`.

### Test executable
A test process is also provided and can be run by the following command:
```console
    make run_tests
```
This code does some cross-intersection as well as self-interpolation tests on meshes of different sizes
then performs a linear regression on the L2 error to check the first order convergence.
It also performs a linear regression on the computation time to check the linear time complexity.
The aim of the self-interpolation test is to check the robustness of the code in a completely degenerated case.

### Robust predicates
The polygon clipping algorithm relies on an orientation test which is performed by a regular cross product by default.
One could use the robust orientation test from Jonathan Richard Shewchuk (see the `predicates.c` file for details) instead.
However, this should not be necessary in any case because the implementation of the polygon clipping algorithm
already features a tolerance to handle numerical issues such as rounding errors.
This feature is enabled by setting `ROBUST_PREDICATES=1`.

## Improvements
Several pieces of code here are far from optimal.

The aim of the Python scripts is only to provide files for the Fortran code to work with and to display the results,
therefore I do not plan to improve them any time soon.

Building the connectivity is also computationally heavy but it can reasonably be assumed that when using
this piece of software as a library in a bigger project, the connectivity is already provided by a fancy mesh reader or by the meshing tool itself (especially if the Voronoï mesh is constructed by duality).

The interpolation function could be optimized further by smartly providing an initial seed instead of brute forcing one.
However, this would be very dependent on the software this interpolation is implemented in.
