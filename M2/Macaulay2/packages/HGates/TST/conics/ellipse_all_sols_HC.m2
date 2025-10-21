
needs "../../HGates.m2"
R = CC_53
declareVariable \ {A, B, C, D, E, x, y, t}

-- 1. Given points
x1 = inputHGate 9_R
y1 = zeroHGate
x2 = minusOneHGate
y2 = zeroHGate
x3 = fourHGate * x - inputHGate 8_R
y3 = inputHGate (-9_R/5)
x4 = inputHGate (4 - sqrt(5)_R * 5/3)
y4 = y * inputHGate (5_R/2) - inputHGate 0.5
x5 = twoHGate * x 
y5 = inputHGate (-3_R) * y

-- 2. For Predictor-Corrector Homotopy Continuation
f0 = method()
f0(HGate, HGate) := (X, Y) -> (
  A*X*X + B*X*Y + C*Y*Y + D*X + E*Y + oneHGate
)
f1 = f0(x1, y1)
f2 = f0(x2, y2)
f3 = f0(x3, y3)
f4 = f0(x4, y4)
f5 = f0(x5, y5)
f6 = A - C - (inputHGate 0.25 * D * D) + (inputHGate 0.25 * E * E) 
f7 = (inputHGate 0.5 * B) - (inputHGate 0.25 * D * E)

X = hMatrixGate({A, B, C, D, E, x, y}, 7, 1)
n = length X
Xlist = X.Elements;
F = hMatrixGate({f1, f2, f3, f4, f5, f6, f7}, 7, 1)
MF = hMap({X}, {F})

-- starting system
g1 = A - oneHGate
g2 = B - oneHGate
g3 = C*C*C - oneHGate
g4 = x*x*x - oneHGate
g5 = y*y*y - oneHGate
g6 = D*D - oneHGate
g7 = E*E - oneHGate

G = hMatrixGate({g1, g2, g3, g4, g5, g6, g7}, 7, 1)
MG = hMap({X}, {G})
d = 0.01 -- time-step

end