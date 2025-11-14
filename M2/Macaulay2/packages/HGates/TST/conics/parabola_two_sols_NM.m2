needs "../../HGates.m2"
R = CC_53
declareVariable \ {x, y}

-- 1. Given points 
x1 = inputHGate (-8_R)
y1 = inputHGate (-21.48779)
x2 = inputHGate (-8_R)
y2 = inputHGate (-0.508075)
x3 = (inputHGate (-4*sqrt(2)/3)) * x - inputHGate (4/3 + 2*sqrt(2))
y3 = zeroHGate
x4 = zeroHGate
y4 = y * inputHGate 16_R - inputHGate (12 + 2*sqrt(2))
x5 = inputHGate 0.5 * x 
y5 = inputHGate (sqrt(2)) * y

-- 2. For Newton's Method
AA = hMatrixGate({
    x1*x1, x1*y1, y1*y1, x1, y1,
    x2*x2, x2*y2, y2*y2, x2, y2,
    x3*x3, x3*y3, y3*y3, x3, y3,
    x4*x4, x4*y4, y4*y4, x4, y4,
    x5*x5, x5*y5, y5*y5, x5, y5}, 
    5, 5)
bb = hMatrixGate({minusOneHGate, minusOneHGate, minusOneHGate, minusOneHGate, minusOneHGate}, 5, 1)
Y = solveHMatrixGate(AA, bb)

(A, B, C, D, E) = apply(0..4,i->elementHGate(Y,i))
F = hMap({hMatrixGate({x, y},2,1)},{
    hMatrixGate({A - C - D*D * inputHGate 0.25 + E * E * inputHGate 0.25, 
                B * inputHGate 0.5 - D * E * inputHGate 0.25},2,1)}) 
end