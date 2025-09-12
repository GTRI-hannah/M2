-- Subsection: Solving Systems of Equations
-- Subsubsection: Homotopy Continuation Problem
-- (reference from previous section, Problem 3)
restart
needs "../HGates.m2"
R = RR_53
declareVariable \ {x, y, t}

-- 1. set up problem, define Newton's Operator
f_1 = x*x - (fiveHGate*x) + sixHGate -- roots: 2, 3
f_2 = y*y*y - (threeHGate*y*y) - (fourHGate*y) + twelveHGate -- roots: 2, -2, 3
g_1 = x*x - oneHGate
g_2 = y*y*y - oneHGate

X = hMatrixGate({x,y}, 2, 1)
n = length X
Xlist = X.Elements;
F = hMatrixGate({f_1, f_2}, 2, 1)
G = hMatrixGate({g_1, g_2}, 2, 1)
H = ((oneHGate - t)*G) + ((inputHGate 0.528)*t*F) -- \gamma based on Frobenius norm
M = hMap({t, X}, {H})

-- 3. traverse homotopy from t = 0 to t = 1
X0literal = {1., 1.}
t0literal = 0.
d = 0.1 -- time step
time while (t0literal < 1.) do (
  t1literal = t0literal + d;
  predlist = toList (t0literal, t1literal, X0literal);

  << "Iteration: " << t1literal << endl;
  -- 1. predict
  X1literal = predictorRK4(M, predlist);
  << "Predicted: " << X1literal << endl;
  
  -- 2. correct
  Msinglevar = subMap(t, inputHGate t1literal, M);
  X1literal = newtonsMethod(Msinglevar, X1literal);
  << "Corrected: " << X1literal << endl;

  -- 3. update values for next iteration
  t0literal = t0literal + d;
  X0literal = X1literal;
)

<< "Solutions found X': " << X1literal << endl;
evalF = toList (specialize(F, inputValueTable {x => X1literal#0, y => X1literal#1}))
<< "Evaluation of target system F(X'): " << evalF << endl;
--<< "Difference with known solution (2, -2) under 2-norm: " << sqrt fold(plus, (X1literal - {2, -2})/(i -> i*i)) << endl;
<< "||F(X')||_2: " << sqrt fold(plus, (evalF)/(i -> i*i)) << endl;
