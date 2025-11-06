needs "../../HGates.m2"
R = CC_53
declareVariable \ {x, y, t}

-- 1. Given points
x1 = inputHGate 9_R
y1 = zeroHGate
x2 = minusOneHGate
y2 = zeroHGate
x3 = fourHGate * x - inputHGate 8_R
y3 = inputHGate (-9/5_R)
x4 = inputHGate (4 - sqrt(5) * 5/3)
y4 = y * inputHGate (5/2) - inputHGate 0.5
x5 = twoHGate * x 
y5 = inputHGate (-3_R) * y

-- 2. For Predictor-Corrector Homotopy Continuation
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
X = hMatrixGate({x, y}, 2, 1)
F = hMatrixGate({A - C - D*D * inputHGate 0.25 + E * E * inputHGate 0.25, 
                B * inputHGate 0.5 - D * E * inputHGate 0.25},2,1)
MF = hMap({X},{F}) 


-- starting system
g1 = (x*x*x*x)*(x*x*x*x)*(x*x*x*x) - oneHGate
g2 = (y*y*y*y)*(y*y*y*y)*(y*y*y*y) - oneHGate

G = hMatrixGate({g1, g2}, 2, 1)
MG = hMap({X}, {G})
Gsol = {1, 1} -- known solution of G
d = 0.05

end