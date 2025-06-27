restart
needs "../HGates.m2"
declareVariable \ {x,y,z,w}
M = hMatrix({x,y,z,w}, 2, 2)
D = detHGate(M)
diff(x,D)

M = hMatrix({x}, 1, 1)
D = detHGate(M)
diff(x,D)

M = hMatrix({x,y,z,w}, 2, 2)
N = hMatrix({oneHGate, y}, 2, 1)
S = solveHGate(M, N)
S.Inputs 
M.Elements 
S.Elements 
diff(x,S)

