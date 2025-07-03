restart
needs "../HGates.m2"
declareVariable \ {x,y,z,w}
M = hMatrix({x}, 1, 1)
N = hMatrix({oneHGate}, 1, 1)
S = solveHGate(M, N)
Sx = diff(x,S) -- should be 1/x^2

M = hMatrix({oneHGate}, 1, 1)
N = hMatrix({x+y}, 1, 1)
S = solveHGate(M, N)
Sx = diff(x,S) -- should be 1

M = hMatrix({x,y,zeroHGate,z}, 2, 2)
N = hMatrix({oneHGate, w}, 2, 1)
S = solveHGate(M, N) 
length S -- should be 2
Sx = diff(x,S) -- should 

