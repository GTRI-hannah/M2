restart
needs "../HGates.m2"
declareVariable \ {x,y,z,w}
R = RR_53; -- using to unify ring for "matrix" function
x0 = inputValueTable {x => 2_R, y => pi_R, z => 0_R, w => 1/2_R}

-- check HGates (not HMatrixGates)
-- net, length, diff, specialize
g_1 = x + x
g_2 = g_1 * y 
g_3 = g_2 * zeroHGate
g_4 = g_3 + z 
g_5 = g_4 * w

-- expect 1 for all
length g_1 
length g_2 
length g_3
length g_4 
length g_5 

diff(x, g_1) -- expect 1+1
diff(z, g_4) -- expect 1
diff(w, g_5) -- expect z
specialize(g_1, x0) -- expect 4
specialize(g_2, x0) -- expect 4*pi
specialize(diff(x, g_5), x0) -- expect 0

printSLP ({x, y, z, w}, {g_1, g_5})

-- check DetHGate
M = hMatrixGate({x, y, z, x}, 2, 2)
g_1 = detHGate(M)
g_2 = diff(x, g_1)
g_3 = elementHGate(M, 0)
elementHGate(g_1, 0) -- expect type error 
diff(x, g_3)

specialize(g_1, x0)
specialize(g_2, x0) 
specialize(g_3, x0)

length M 
length g_1 
length g_3 

-- check HMatrixGate
M = hMatrixGate({x, y, z, w}, 2, 2 )
N = hMatrixGate({x, y}, 2, 1)
O = hMatrixGate({x, y, z, oneHGate}, 2, 2)
g_1 = solveHMatrixGate(M, N)
g_2 = productHMatrixGate(M, g_1)
g_3 = sumHMatrixGate(N, g_2)
g_4 = sumHMatrixGate(M, O)
g_5 = scalarProductHMatrixGate(x, M)

diff(x, M)
diff(x, g_1)
diff(y, g_3)
dM = diff(x, M)
dO = diff(x, O)
sumHMatrixGate(dM, dO) -- not sure why this is printing M + O
diff(x, g_4) -- not sure why this is printing M + O
g_6 = diff(x, g_5) 

specialize(g_3, x0 )
specialize(g_4, x0) 
specialize(sumHMatrixGate(dM, dO), x0) -- evaluation correct
specialize(g_1, x0) 
specialize(g_5, x0)

printSLP ({x, y, z, w}, {g_1, g_4, g_5})

-- check sub 
restart
needs "../HGates.m2"
declareVariable \ {x,y,z,w}
R = RR_53; -- using to unify ring for "matrix" function
x0 = inputValueTable {x => 2_R, y => pi_R, z => 0_R, w => 1/2_R}

subGate (x, y, x)
subGate (x, y, z)
subGate (x, y, y+z)
subGate (x, y, x*y)
M = hMatrixGate({x, y, z, w}, 2, 2 )
subGate (x, y, M)
N = hMatrixGate({x, y}, 2, 1)
O = hMatrixGate({x, y, z, oneHGate}, 2, 2)
g_1 = solveHMatrixGate(M, N)
subGate (x, y, g_1)
g_2 = productHMatrixGate(M, g_1)
subGate (x, y, g_2)
g_4 = sumHMatrixGate(M, O)
subGate (x, y, g_4)
g_5 = scalarProductHMatrixGate(x, M)
subGate (x, y, g_5)

-- check predicate
restart
needs "../HGates.m2"
declareVariable \ {x,y,z,w,t, s_0, s_1, y}
R = RR_53; -- using to unify ring for "matrix" function

-- example with f = 5xt
T0 = hMatrixGate({s_0, s_1}, 2, 1)
x0 = inputHGate 2
X0 = hMatrixGate({y}, 1, 1)
H = hMatrixGate({T0, X0}, 2, 1)

h2 = (inputHGate 5)*t 
h3 = h2 * x
F = hMatrixGate({h3}, 1, 1)
X = hMatrixGate({x}, 1, 1)
p = predictorHMatrixGate(H, t, X, F)

L = inputValueTable {y => 2_R, s_0 => 1_R, s_1 => 1.1_R}
specialize(p, L)

