restart
needs "../HGates.m2"
declareVariable \ {x, y}
a = x + x 
b = a + a 
e = x + y
m = hMatrixGate({x, a, b, e}, 2, 2)
d = detHMatrixGate(m)
n = hMatrixGate({x, y, b, y}, 2, 2)
f = detHMatrixGate(n)
g = f + d
h = hMatrixGate({x, y}, 2, 1)
i = solveHMatrixGate(n, h)
j = bigSumHMatrixGate(h, i)
k = bigProductHMatrixGate(m, j)
l = elementHMatrixGate(k, 0)
printSLP ({x, y}, {l})