restart
needs "../HGates.m2"
declareVariable \ {x, y, t, x0, y0, t0, t1}
R = RR_53; -- using to unify ring for "matrix" function

-- set up map
G = hMatrixGate({(x*x+fiveHGate*x-sixHGate)*t, (y*y*y+twoHGate*y-oneHGate)*t}, 2, 1)
X = hMatrixGate({x, y}, 2, 1)
MG = hMap({t, X}, {G})

-- initial value variables 
X0 = hMatrixGate({x0, y0}, 2, 1)

-- predictor tangent
pTangent = predictorTangHMatrixGate(MG, {t0, t1, X0})

-- fix time t0, t1
pTangentFixTime = subGate(t0, inputHGate 0.2, subGate(t1, inputHGate 0.3, pTangent))

-- make map for Newton's Operator
pTangentMap = hMap({X0}, {pTangentFixTime})
specialize(newtonsOp(pTangentMap), inputValueTable {x0 => 1., y0 => 5.}) --{.333333, 1.808}

pTrap = predictorTrapHMatrixGate(MG, {t0, t1, X0})
pTrapFixTime = subGate(t0, inputHGate 0.2, subGate(t1, inputHGate 0.3, pTrap))
pTrapMap = hMap({X0}, {pTrapFixTime})
specialize(newtonsOp(pTrapMap), inputValueTable {x0 => 1., y0 => 5.}) -- {.6, 3.0744}

pRK4 = predictorRK4HMatrixGate(MG, {t0, t1, X0})
pRK4FixTime = subGate(t0, inputHGate 0.2, subGate(t1, inputHGate 0.3, pRK4))
pRK4Map = hMap({X0}, {pRK4FixTime})
specialize(newtonsOp(pRK4Map), inputValueTable {x0 => 1., y0 => 5.}) -- {.333333, 2.08289}


----------SCRAP--------------------
-- example with H = 5x^2(1-t) + (5x^2+2x+1)t
-- using y_0, s_i instead of x_0, t_i bc of M2 variable naming conventions
-- y_0 = 0, s_0 = 0, s_1 = 0.1
T0 = hMatrixGate({s_0, s_1}, 2, 1)
X0 = hMatrixGate({y_0}, 1, 1)
initVars = hMatrixGate({T0, X0}, 2, 1)

h = (inputHGate 5)*x*x*(oneHGate - t) + ((inputHGate 5)*x*x + (inputHGate 2)*x + oneHGate)*t
H = hMatrixGate({h}, 1, 1)
X = hMatrixGate({x}, 1, 1)
p = predictorHMatrixGate(initVars, t, X, H)
p' = subGate(s_1,inputHGate 0.1, subGate(s_0,zeroHGate,p))
-- 1. define Newto n's Operator
g_1 = hMap({hMatrixGate({y_0},1,1)},{p'-hMatrixGate({inputHGate 2.5},1,1)})
specialize(g_1, inputValueTable {y_0 => 1.})
g_2 = newtonsOp(g_1)

-- 2. pick initial value
x0 = 1.
L = inputValueTable {y_0 => x0}

-- 3. check one application of Newton's Operator
x1 = (specialize (g_2, L))#0

-- 4. iterate over x_k until we find one satisfying
-- |x_k - x_{k-1}| < 0.1
track = 0 -- track iterations
while (abs(x0-x1) >= 0.001) do (
    x0 = x1;
    L = inputValueTable {y_0 => x0};
    x1 = (specialize (g_2, L))#0;
    track = track + 1
)

<< "Result: " << x1 << ", found after " << track << " iterations" << endl;
x0 = x1;
L = inputValueTable {y_0 => x0};
specialize (p', L) - {2.5}
