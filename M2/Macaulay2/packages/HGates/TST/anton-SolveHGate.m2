restart
needs "../HGates.m2"
declareVariable \ {x,y,z,w}
M = hMatrix({x}, 1, 1)
N = hMatrix({oneHGate}, 1, 1)
S = solveHGate(M, N)

M = hMatrix({x,y,z,w}, 2, 2)
N = hMatrix({oneHGate, oneHGate}, 2, 1)
S = solveHGate(M, N) 
length S
Sx = diff(x,S)

