restart
needs "../HGates.m2"
declareVariable \ {a,b,x,y,z,w}
R = RR; -- using to unify ring for "matrix" function
x0 = inputValueTable {x => 2_R, y => pi_R, z => 0_R, w => 1/2_R, a => 1, b => -1}

-- input, sum, product
specialize(x,x0)
specialize(x+y,x0)
specialize(x+y+z+w, x0)
p = specialize(x*y, x0)
specialize(x*y+z, x0)

-- matrix, determinant, matrix element, matrix sum, matrix product
M = hMatrixGate({x,z,w,y}, 4)
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

N = hMatrixGate({x, x, x, x}, 4)
O = hMatrixGate({x, b, b, a}, 4)
Q = hMatrixGate({x, x}, 2)
specialize(bigSumHMatrixGate(M, N), x0)
specialize(diff (x, bigSumHMatrixGate(M, N)), x0)

specialize(bigProductHMatrixGate({2, 2, 2}, M, N), x0)
specialize(bigProductHMatrixGate({2, 2, 2}, M, O), x0)
specialize(diff (x, bigProductHMatrixGate({2, 2, 2}, M, N)), x0)

specialize(solveHMatrixGate(O, Q), x0)
specialize(diff (x, solveHMatrixGate(O, Q)), x0)

restart
needs "../HGates.m2"
declareVariable \ {x, y, z}
R = RR; -- using to unify ring for "matrix" function
x0 = inputValueTable {x => 2_R, y => pi_R}

H = hashTable{1 => (inputHMatrixGate, {x}), 
               2 => (inputHMatrixGate, {y}),
               3 => (sumHMatrixGate, {1, 2}), 
               4 => (productHMatrixGate, {1, 2}),
               5 => (detHMatrixGate, {1, 2, 3, 4})
            };
I = {1, 2};
O = {5}
P = hSLP(H, I, O)
length P 
sizeSLP P
values P.Graph
specialize (P.Graph#1, x0)
specialize (P, x0)

-- PROBLEM
gate = inputHMatrixGate x 
var = ((H#1)#1)#0
gate === x -- FALSE!
var === x

