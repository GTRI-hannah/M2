restart
needs "../HGates.m2"
declareVariable \ {x,y,z,w}
M = hMatrixGate({x},1)
N = hMatrixGate({oneHMatrixGate}, 1) -- oneHMatrixGate is a constant
S = solveHMatrixGate(M, N)
Sx = diff(x,S) -- should be 1/x^2

M = hMatrixGate({oneHMatrixGate}, 1)
N = hMatrixGate({x+y}, 1)
S = solveHMatrixGate(M, N)
Sx = diff(x,S) -- should be 1

M = hMatrixGate({x,y,zeroHMatrixGate,z}, 2)
N = hMatrixGate({oneHMatrixGate, w}, 2)
S = solveHMatrixGate(M, N) 
assert(length S == 2) -- wrong, should be 2
Sx = diff(x,S) 
