-- check predicate
restart
needs "../HGates.m2"
declareVariable \ {s_0, s_1, y_0, x, t}
R = RR_53; -- using to unify ring for "matrix" function

-- example with H = 5x^2t + (5x^2+2x+1)(1-t)
-- using y_0, s_i instead of x_0, t_i bc of M2 variable naming conventions
-- y_0 = 0, s_0 = 0, s_1 = 0.1
T0 = hMatrixGate({s_0, s_1}, 2, 1)
X0 = hMatrixGate({y_0}, 1, 1)
initVars = hMatrixGate({T0, X0}, 2, 1)

h = (inputHGate 5)*x*x*t + ((inputHGate 5)*x*x + (inputHGate 2)*x + oneHGate)*(oneHGate - t)
H = hMatrixGate({h}, 1, 1)
X = hMatrixGate({x}, 1, 1)
p = predictorHMatrixGate(initVars, t, X, H)

showStructure p

L = inputValueTable {s_0 => 0_R, s_1 => 0.1_R, y_0 => 0_R}
predicted = specialize(p, L) 

M = inputValueTable {t => 0_R, x => 0_R}
cAbstract = c (t, X, H)
c0 = specialize (cAbstract, M) -- expect 0.5

M = inputValueTable {t => 0.1_R, x => 0.05_R}
c1 = specialize (cAbstract, M) -- expect 0.478

predicted#0 == (c0#0 + c1#0) * (0.1*0.5) -- hooray!