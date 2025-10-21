-- Conic 19
-- {-0.635, 0.348, -0.737, 0.964, 0.723, 1.601, 2.281}
-- {-0.635, 0.348, -0.737, 0.964, 0.723, 0.6191, 2.1333}
needs "../../HGates.m2"
R = RR_53
declareVariable \ {A, B, C, D, E, x, y, t}

-- 1. Given points
x1 = inputHGate (-0.2969)
y1 = inputHGate (-0.6137)
x2 = inputHGate (1.9384)
y2 = inputHGate (2.194)
x3 = (inputHGate (-2.986454832467665)) * x - inputHGate (4.0739141867807325)
y3 = zeroHGate
x4 = zeroHGate
y4 = y * inputHGate (-17.110358835477328) - inputHGate (38.25522850372379)
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
