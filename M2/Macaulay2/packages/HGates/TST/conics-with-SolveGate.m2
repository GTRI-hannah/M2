restart
needs "../HGates.m2"
R = RR_53
declareVariable \ {A, B, C, D, E, x, y}

-- given points
(x1, y1) = (zeroHGate, twoHGate)
(x2, y2) = (twoHGate, zeroHGate)
(x3, y3) = (minusOneHGate * twoHGate, y)
(x4, y4) = (x, minusOneHGate * twoHGate)
(x5, y5) = (oneHGate+x, (inputHGate sqrt(3))+y)

G3 = hMatrixGate({fourHGate*A - fourHGate*C - D*D + E*E, twoHGate*B - D*E}, 2, 1)
Z = hMatrixGate({D, E}, 2, 1)
-- initial values for D, E 
D0 = oneHGate
E0 = oneHGate
Z0 = hMatrixGate({D0, E0}, 2, 1)
dG3dZ = jacobian (Z, G3)
dG3dZatZ0 = subGate(E, E0, subGate(D, D0, dG3dZ))
ZRHS = dG3dZatZ0*Z0 - subGate(E, E0, subGate(D, D0, G3))
ZRHSvec = hMatrixGate({elementHGate(ZRHS, 0), elementHGate(ZRHS, 1)}, 2, 1) -- making formatting work
Z1 = solveHMatrixGate(dG3dZatZ0, ZRHSvec)

-- MAY POSSIBLY HAVE ISSUES WITH REFERENCING Z1
g3 = A*x3*x3 + B*x3*y3 + C*y3*y3 + elementHGate(Z1, 0)*x3 + elementHGate(Z1, 1)*y3 + oneHGate
g4 = A*x4*x4 + B*x4*y4 + C*y4*y4 + elementHGate(Z1, 0)*x4 + elementHGate(Z1, 1)*y4 + oneHGate
g5 = A*x5*x5 + B*x5*y5 + C*y5*y5 + elementHGate(Z1, 0)*x5 + elementHGate(Z1, 1)*y5 + oneHGate
G2 = hMatrixGate({g3, g4, g5}, 3, 1)
Y = hMatrixGate({A, B, C}, 3, 1)
-- initial values for A, B, C
A0 = oneHGate
B0 = zeroHGate
C0 = fiveHGate 
Y0 = hMatrixGate({A0, B0, C0}, 3, 1)
dG2dY = jacobian (Y, G2)
dG2dYatY0 = subGate(A, A0, subGate(B, B0, subGate(C, C0, dG2dY)))
YRHS = dG2dYatY0*Y0 - subGate(A, A0, subGate(B, B0, subGate(C, C0, G2)))
YRHSvec = hMatrixGate({elementHGate(YRHS, 0), elementHGate(YRHS, 1), elementHGate(YRHS, 2)}, 3, 1) -- make formmating work
Y1 = solveHMatrixGate(dG2dYatY0, YRHSvec)

g11 = A*x1*x1 + B*x1*y1 + C*y1*y1 + elementHGate(Z1, 0)*x1 + elementHGate(Z1, 1)*y1 + oneHGate;
g1 = subGate(C, elementHGate(Y1, 2), subGate(B, elementHGate(Y1, 1), subGate(A, elementHGate(Y1, 0), g11)));
g21 = A*x2*x2 + B*x2*y2 + C*y2*y2 + elementHGate(Z1, 0)*x2 + elementHGate(Z1, 1)*y2 + oneHGate;
g2 = subGate(C, elementHGate(Y1, 2), subGate(B, elementHGate(Y1, 1), subGate(A, elementHGate(Y1, 0), g21)));
G1 = hMatrixGate({g1, g2}, 2, 1);
X = hMatrixGate({x, y}, 2, 1);
H = hMap({X}, {G1});

X1 = newtonsMethod(H, {1., 8.})

