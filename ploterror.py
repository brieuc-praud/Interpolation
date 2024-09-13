#!/usr/bin/env python3

"""
    plot the error of interpolation and do a linear regression to check the order 1
"""

import sys
import matplotlib.pyplot as plt
import numpy as np

if len(sys.argv)-1 != 1:
     sys.stderr.write("usage: " + sys.argv[0] + " ERROR_FILE\n")
     sys.exit()

error_file = sys.argv[1] # the file containing the source of the interpolation

with open(error_file, 'r') as f:
    content = f.read().splitlines()
X = []
Y = []
for line in content:
    x, y = line.split()
    X.append(int(x))
    Y.append(float(y))

# the characteristic length of a cell is proportional to the square root of its area
# which is itself proportional to the inverse of the number of cells
X = -.5*np.log(X[1:])
Y = np.log(Y[1:])

reg  = np.polyfit(X, Y, 1)
line = np.poly1d(reg)

plt.plot(X, Y, 'ro')
plt.plot(X, line(X), '-b')
plt.title(f"Linear regression of slope {reg[0]:.2f}")
plt.xlabel(r"$-\frac{1}{2}\log(\mathrm{cells\ number})$")
plt.ylabel(r"$\log(\mathrm{L2\ error})$")
plt.show()
