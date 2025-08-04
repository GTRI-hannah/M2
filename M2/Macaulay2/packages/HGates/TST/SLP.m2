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

restart
needs "../HGates.m2"
declareVariable \ {x, y}
a = x + x
b = a + y
c = hMatrixGate({b}, 1, 1)
d = inputHMatrixGate 3 
e = c + d 
printSLP ({x, y}, {e})

a = x + 3
b = hMatrixGate({a, x, y, y}, 2, 2)
c = hMatrixGate({x, y}, 2, 1)
d = bigProductHMatrixGate(b, c)
printSLP ({x, y}, {d})

