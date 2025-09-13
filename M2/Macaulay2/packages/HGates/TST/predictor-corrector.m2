-- Subsection: Solving Systems of Equations
-- Subsubsection: Homotopy Continuation Problem
-- (reference from previous section, Problem 3)
restart
needs "../HGates.m2"
R = RR_53
declareVariable \ {x, y, t}

-- 1. Define HMap of F
f_1 = x*x - (fiveHGate*x) + sixHGate -- roots: 2, 3
f_2 = y*y*y - (threeHGate*y*y) - (fourHGate*y) + twelveHGate -- roots: 2, -2, 3

X = hMatrixGate({x,y}, 2, 1)
n = length X
Xlist = X.Elements;
F = hMatrixGate({f_1, f_2}, 2, 1)
MF = hMap({X}, {F})
g_1 = x*x - oneHGate
g_2 = y*y*y - oneHGate
G = hMatrixGate({g_1, g_2}, 2, 1)
MG = hMap({X}, {G})
Gsol = {1., 1.}
d = 0.1

X1literal = predictorCorrector(MF, MG, Gsol d)

<< "Solutions found X': " << X1literal << endl;
evalF = toList (specialize(F, inputValueTable {x => X1literal#0, y => X1literal#1}))
<< "Evaluation of target system F(X'): " << evalF << endl;
--<< "Difference with known solution (2, -2) under 2-norm: " << sqrt fold(plus, (X1literal - {2, -2})/(i -> i*i)) << endl;
<< "||F(X')||_2: " << sqrt fold(plus, (evalF)/(i -> i*i)) << endl;
