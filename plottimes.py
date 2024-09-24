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
X  = []
Ys = []
Yc = []
for line in content:
    x,ys,yc = line.split()
    X.append(int(x))
    Ys.append(float(ys))
    Yc.append(float(yc))

X  = np.log(X)
Ys = np.log(Ys)
Yc = np.log(Yc)

regs = np.polyfit(X, Ys, 1)
regc = np.polyfit(X, Yc, 1)
lines = np.poly1d(regs)
linec = np.poly1d(regc)

plt.plot(X, Ys, 'r+')
plt.plot(X, Yc, 'b+')
plt.plot(X, lines(X), '-r', label=r"self-intersection: $y = {{{a:.2f}}}\,x\,{{{b:+.2f}}}$".format(a=regs[0], b=regs[1]))
plt.plot(X, linec(X), '-b', label=r"cross-intersection: $y = {{{a:.2f}}}\,x\,{{{b:+.2f}}}$".format(a=regc[0], b=regc[1]))
plt.title(r"Time complexity: $O(n^{{{:.2f}}})$".format(regc[0]))
plt.xlabel(r"$x = \log(\mathrm{cells\ number})$")
plt.ylabel(r"$y = \log(\mathrm{computation\ time})$")
plt.legend()
plt.show()
