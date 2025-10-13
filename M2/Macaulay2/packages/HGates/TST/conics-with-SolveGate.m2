needs "../HGates.m2"
R = RR_53
declareVariable \ {A, B, C, D, E, x, y}

-- given points
-- (x, y) = (1, 1)
--(x1, y1) = (inputHGate 8 + x, y + minusOneHGate) 
--(x2, y2) = (inputHGate 5.545, inputHGate (2.853) * y)
--(x3, y3) = (inputHGate (-0.045) * x, inputHGate (1.763)) 
--(x4, y4) = (inputHGate (-0.045), inputHGate (-1.763))
--(x5, y5) = (inputHGate 5.545, inputHGate (-2.853))

-- (x, y) = (0, 1)
--(x1, y1) = (inputHGate (-2), zeroHGate)
--(x2, y2) = (twoHGate, zeroHGate)
--(x3, y3) = (x, twoHGate)
--(x4, y4) = (x, minusOneHGate * twoHGate * y)
--(x5, y5) = (inputHGate sqrt(3), y)

-- (x, y) = (2, 3)
(x1, y1) = (zeroHGate, y)
(x2, y2) = (x-twoHGate, inputHGate (-3/7)*y)
(x3, y3) = (inputHGate (2*sqrt(3)), twoHGate + inputHGate (5*sqrt(3/7)))
(x4, y4) = (inputHGate (2*sqrt(3)) + inputHGate (15/sqrt(13)), twoHGate)
(x5, y5) = (x * inputHGate sqrt(3) - inputHGate (15/sqrt(13)), twoHGate)

AA = hMatrixGate({
    x1*x1, x1*y1, y1*y1, x1, y1,
    x2*x2, x2*y2, y2*y2, x2, y2,
    x3*x3, x3*y3, y3*y3, x3, y3,
    x4*x4, x4*y4, y4*y4, x4, y4,
    x5*x5, x5*y5, y5*y5, x5, y5
    }, 5, 5)
bb = hMatrixGate({minusOneHGate, minusOneHGate, minusOneHGate, minusOneHGate, minusOneHGate}, 5, 1)
ABCDE = solveHMatrixGate(AA, bb)

(A,B,C,D,E) = apply(0..4,i->elementHGate(ABCDE,i))
focusAtOrigin = {A-C-D*D*inputHGate 0.25+E*E*inputHGate 0.25, inputHGate .5*B-inputHGate 0.25*D*E}
F = hMap({hMatrixGate({x,y},2,1)},{hMatrixGate(focusAtOrigin,2,1)}) 
end

restart
load "conics-with-SolveGate.m2"
specialize(AA, inputValueTable {x=> 1.1, y=> 1.1})
MM = matrix {{4, 0, 0, -2, 0}, {4, 0, 0, 2, 0}, {0, 0, 4, 0, 2}, {0, 0, 4, 0, -2}, {3, 1.73205, 1, 1.73205, 1}}
--MM = matrix {{82.81, .91, .01, 9.1, .1}, {30.747, 17.4019, 9.84893, 5.545, 3.1383}, {.00245025, -.0872685, 3.10817, -.0495, 1.763}, {.002025, .079335, 3.10817, -.045, -1.763}, {30.747, -15.8199, 8.13961, 5.545, -2.853}}
SVD MM
--specialize(hMatrixGate(focusAtOrigin,2,1), inputValueTable {x=> 1.1, y=> 3.1})

-- try Newton
X1 = newtonsMethod(F, {4.67727, 17.8568}) 

specialize(F, inputValueTable {x=> X1#0, y=> X1#1})

-- intervals?
newtonsMethod(F, {interval[.9,1.1],interval[0.9,1.1]})

