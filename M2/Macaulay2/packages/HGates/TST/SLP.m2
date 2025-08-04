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
printSLP ({x, y}, {e, a})

-- test 1, arithmetic gates (for C compiler)
restart
needs "../HGates.m2"
declareVariable \ {x, y}
a = x + x
b = a * x
c = hMatrixGate({y}, 1, 1)
d = c + b 
d.Cols 
a.Rows 
e = bigProductHMatrixGate(d, a)
f = bigSumHMatrixGate(e, c)
printSLP ({x, y}, {e, f})

-- test 2, scalars and determinant gate (for C compiler)
restart
needs "../HGates.m2"
declareVariable \ {x, y}
a = inputHMatrixGate 3
b = hMatrixGate({x, a, y, 0}, 2, 2)
c = detHMatrixGate(b)
d = c + a
printSLP ({x, y}, {d})


