restart
needs "../HGates.m2"
declareVariable \ {a,b,x,y,z,w}

R = RR_53; -- using to unify ring for "matrix" function
x0 = inputValueTable {x => 2_R, y => pi_R, z => 0_R, w => 1/2_R, a => 1, b => -1}

--R = frac(QQ[A,B,X,Y,Z,W]); -- using to unify ring for "matrix" function
--x0 = inputValueTable {x => X, y => Y, z => Z, w => W, a => A, b => B}

-- input, sum, product
specialize(x,x0)
specialize(x+y,x0)
specialize(x+y+z+w, x0)
p = specialize(x*y, x0)
specialize(x*y+z, x0)

-- matrix, determinant, matrix element, matrix sum, matrix product
M = hMatrixGate({x,z,w,y}, 2, 2)
specialize(M, x0)
detG = detHMatrixGate(M)
d = specialize(detG, x0)
assert (d == p)

diffxDetG = diff(x, detG)
diffyDetG = diff(y, detG)
specialize(diffxDetG, x0)
specialize(diffyDetG, x0)

specialize(elementHMatrixGate(M, 0), x0)
specialize(diff (x, elementHMatrixGate(M, 0)), x0)

N = hMatrixGate({x, x, x, x}, 2, 2)
O = hMatrixGate({x, b, b, a}, 2, 2)
Q = hMatrixGate({x, x}, 2, 1)
specialize(bigSumHMatrixGate(M, N), x0)
specialize(diff (x, bigSumHMatrixGate(M, N)), x0)

specialize(bigProductHMatrixGate(M, N), x0)
specialize(bigProductHMatrixGate(M, O), x0)
specialize(diff (x, bigProductHMatrixGate(M, N)), x0)

specialize(solveHMatrixGate(O, Q), x0)
specialize(diff (x, solveHMatrixGate(O, Q)), x0)


