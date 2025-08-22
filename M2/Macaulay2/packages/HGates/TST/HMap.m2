-- check HMap
restart
needs "../HGates.m2"
declareVariable \ {x, y, z}

g_1 = x * y 
g_2 = g_1 * g_1
h_1 = hMap({x}, {g_2})
h_2 = hMap({x, y}, {g_2})
h_3 = hMap({x}, {hMatrixGate({g_1, g_2}, 2, 1)})

-- testing substitute
subMap (x, z, h_2)

-- testing subtract 
A = hMatrixGate({x, y, x, zeroHGate}, 2, 2)
b = hMatrixGate({z, z}, 2, 1)
c = solveHMatrixGate(A, b)
G = c - b

H = hMap({z}, {G})
X = hMatrixGate({x, y, z}, 3, 1)
instance(x, InputHGate)


-- testing jacobian
jacobian (X, G)
F = hMatrixGate({x+y, x*z, g_2}, 3, 1)
jacobian (X, F)

-- testing newtonsOp
testMap = hMap({X}, {F})
newtonsOp(testMap)