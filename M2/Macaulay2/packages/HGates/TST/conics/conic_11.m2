-- Conic 11
-- {-0.264, 0.196, -0.310, -0.705, -0.556, -4.5657, -3.4922}
-- {-0.264, 0.196, -0.310, -0.705, -0.556, 0.3418, 0.9348}
needs "../../HGates.m2"
R = RR_53
declareVariable \ {A, B, C, D, E, x, y, t}

-- 1. Given points
x1 = inputHGate (-2.5671)
y1 = inputHGate (0.8171)
x2 = inputHGate (-3.0371)
y2 = inputHGate (-4.2437)
x3 = (inputHGate 0.9628120224146713) * x - inputHGate (0.6965108507386644)
y3 = zeroHGate
x4 = zeroHGate
y4 = y * inputHGate (0.9059859950304947) - inputHGate (0.2634842918454936)
x5 = x
y5 = y
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
Gsol = {1, 1, 1, 1, 1, 1, 1} -- known solution of G
d = 0.01 -- time-step

end
