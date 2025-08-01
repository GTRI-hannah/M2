restart
needs "../HGates.m2"
declareVariable \ {x, y}
a = x + x 
b = a + a 
e = x + y
m = hMatrixGate({x, a, b, e}, 4)
d = detHMatrixGate(m)
n = hMatrixGate({x, y, b, y}, 4)
f = detHMatrixGate(n)
g = f + d
printSLP ({x, y}, {g})