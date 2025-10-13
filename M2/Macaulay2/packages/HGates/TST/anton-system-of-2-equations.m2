needs "../HGates.m2"
R = RR_53
declareVariable \ {A, B, C, D, E, x, y}

-- given points
sqrt3 = inputHGate sqrt 3
(x1, y1) = (zeroHGate, twoHGate - sqrt3) 
(x2, y2) = (oneHGate, zeroHGate - sqrt3)
(x3, y3) = (minusOneHGate * oneHGate, y - sqrt3)
(x4, y4) = (x, minusOneHGate * twoHGate - sqrt3)
(x5, y5) = (inputHGate 0.5 * (oneHGate+x), y)

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
focusAtOrigin = {A-C-D*D*inputHGate 0.25+E*E*inputHGate 0.25, B-inputHGate 0.5*D*E}
F = hMap({hMatrixGate({x,y},2,1)},{hMatrixGate(focusAtOrigin,2,1)}) 
end

restart
load "anton-system-of-2-equations.m2"
-- evaluate Jacobian
M = hMatrixGate(focusAtOrigin,2,1)
J = jacobian(hMatrixGate({x,y},2,1),M);

M0 = specialize(M, inputValueTable{x=>0.,y=>0.})
M1 = specialize(M, inputValueTable{x=>0.0001,y=>0.0001})
J0 = matrix pack(2,specialize(J, inputValueTable{x=>0.,y=>0.}))
J1 = matrix pack(2,specialize(J, inputValueTable{x=>0.01,y=>0.01}))

-- evaluate F
F0 = specialize(F, inputValueTable{x=>0.,y=>0.})
F1 = specialize(F, inputValueTable{x=>0.01,y=>0.01})


-- try Newton
newtonsMethod(F, {0.01,0.001})

-- intervals?
newtonsMethod(F, {interval[-.000001,0.000001],interval[-0.000001,0.000001]})

