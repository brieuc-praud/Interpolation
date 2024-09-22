#!/usr/bin/env python3

"""
    plot the computation times of the interpolation and do a linear regression to check the O(n) time complexity
"""

import sys
import matplotlib.pyplot as plt
import numpy as np

if len(sys.argv)-1 != 1:
     sys.stderr.write("usage: " + sys.argv[0] + " TIMES_FILE\n")
     sys.exit()

times_file = sys.argv[1] # the file containing the computation times

with open(times_file, 'r') as f:
    content = f.read().splitlines()
X = []
Y = []
for line in content:
    x, y = line.split()
    X.append(int(x))
    Y.append(float(y))

X = np.log(X[1:])
Y = np.log(Y[1:])

reg  = np.polyfit(X, Y, 1)
line = np.poly1d(reg)

plt.plot(X, Y, 'ro')
plt.plot(X, line(X), '-b')
plt.legend(["_",r"$y = {{{a:.2f}}}\,x\,{{{b:+.2f}}}$".format(a=reg[0], b=reg[1])])
plt.title(r"Time complexity: $O(n^{{{:.2f}}})$".format(reg[0]))
plt.xlabel(r"$x = \log(\mathrm{cells\ number})$")
plt.ylabel(r"$y = \log(\mathrm{computation\ time})$")
plt.show()
