-- check HMap
restart
needs "../HGates.m2"
declareVariable \ {x, y, z}

g_1 = x * y 
g_2 = g_1 * g_1
h_1 = hMap({x}, {g_2})
h_2 = hMap({x, y}, {g_2})
h_3 = hMap({x}, {hMatrixGate({g_1, g_2}, 2, 1)})

-- testing substitute
subMap (x, z, h_2)

-- testing subtract 
A = hMatrixGate({x, y, x, zeroHGate}, 2, 2)
b = hMatrixGate({z, z}, 2, 1)
c = solveHMatrixGate(A, b)
G = c - b

H = hMap({z}, {G})
X = hMatrixGate({x, y, z}, 3, 1)
instance(x, InputHGate)


-- testing jacobian
jacobian (X, G)
F = hMatrixGate({x+y, x*z, g_2}, 3, 1)
jacobian (X, F)

-- testing newtonsOp
testMap = hMap({X}, {F})
newtonsOp(testMap)


-- numeric example (see TST/predictor.m2 for forward direction on y_1)
restart
needs "../HGates.m2"
declareVariable \ {s_0, s_1, y_1, y_0, x, t}
R = RR_53; -- using to unify ring for "matrix" function

-- example with H = 5x^2(t-1) + (5x^2+2x+1)t
-- using y_0, y_1, s_i instead of x_0, x_target, t_i bc of M2 variable naming conventions
T0 = hMatrixGate({s_0, s_1}, 2, 1)
X0 = hMatrixGate({y_0}, 1, 1)
initVars = hMatrixGate({T0, X0}, 2, 1)

h = (inputHGate 5)*x*x*(oneHGate - t) + ((inputHGate 5)*x*x + (inputHGate 2)*x + oneHGate)*t
H = hMatrixGate({h}, 1, 1)
X = hMatrixGate({x}, 1, 1)
predictor = predictorHMatrixGate(initVars, t, X, H)
X1 = hMatrixGate({y_1}, 1, 1)
P = predictor - X1

-- starting point
mapOfH = hMap({X}, {subGate(t, s_0, H)})
startX0 = subGate(x, y_1, newtonsOp(mapOfH))

g = hMap({X0}, {P})
-- use Newton's method to estimate starting point to get y_1
endX0 = subGate(y_0, elementHGate(startX0, 0), newtonsOp(g))

-- y_1 = -.25, s_0 = 0, s_1 = 0.1
L = inputValueTable {s_0 => 0_R, s_1 => 0.1_R, y_1 => -.25_R}
specialize (endX0, L) -- actual solution is 0, this is 0.411

restart
needs "../HGates.m2"
declareVariable \ {x_0, x}
f = x*x - (inputHGate 2)
X = hMatrixGate({x}, 1, 1)
F = hMatrixGate({f}, 1, 1)
mapOff = hMap({X}, {F})

g = newtonsOp(mapOff)

R = RR_53
L = inputValueTable {x => 1.1_R}
specialize (g, L)


-- UNIVARIATE CASE
restart
needs "../HGates.m2"
declareVariable \ {s_0, s_1, y_0, x, t}
R = RR_53; -- using to unify ring for "matrix" function
-- example with H = x^2t + (x^2-5x+6)(1-t)
-- using y_0, s_i instead of x_0, t_i bc of M2 variable naming conventions
T0 = hMatrixGate({s_0, s_1}, 2, 1)
X0 = hMatrixGate({y_0}, 1, 1)
initVars = hMatrixGate({T0, X0}, 2, 1)

h = x*x*(oneHGate-t) + (x*x - (inputHGate 5)*x + (inputHGate 6))*t
H = hMatrixGate({h}, 1, 1)
X = hMatrixGate({x}, 1, 1)
p = predictorHMatrixGate(initVars, t, X, H)

-- try with start 0, time step 1/steps
startvalue = 0
steps = 100
for i from 1 to steps do (
    a = (1/steps)*(i-1);
    b = (1/steps)*i;
    L1 = inputValueTable {s_0 => a_R, s_1 => b_R, y_0 => startvalue_R};
    nextvalue = (specialize(p, L1))#0;

    -- correct with Newton's method n times
    n = 5;
    for j from 1 to n do (
        L2 = inputValueTable {t => b_R, x => nextvalue_R};
        betternextvalue = (specialize(newtonsOp(hMap({X}, {H})), L2))#0;
        nextvalue = betternextvalue;
    );
    startvalue = betternextvalue;
)

betternextvalue -- possible solutions are 2, 3

-- MULTIVARIATE CASE
restart
needs "../HGates.m2"
declareVariable \ {t0, t1, x0, y0, z0, x, y, z, t}
R = RR_53; -- using to unify ring for "matrix" function
-- F = (x^3 - 3x^2 + 2x, y^2 - 2y -1, z^4 - 5z^3 + 6z^2)
T0 = hMatrixGate({t0, t1}, 2, 1)
X0 = hMatrixGate({x0, y0, z0}, 3, 1)
initVars = hMatrixGate({T0, X0}, 2, 1)

F = hMatrixGate({x*x*x - (inputHGate 3)*x*x + (inputHGate 2)*x, 
    y*y - (inputHGate 2)*y - oneHGate, 
    z*z*z*z - (inputHGate 5)*z*z*z + (inputHGate 6)*z*z}, 3, 1)
G = hMatrixGate({x*x*x - oneHGate, 
    y*y - oneHGate, 
    z*z*z*z - oneHGate}, 3, 1)
H = sumHMatrixGate(scalarProductHMatrixGate(oneHGate - t, G), scalarProductHMatrixGate(t, F))
X = hMatrixGate({x, y, z}, 3, 1)
p = predictorHMatrixGate(initVars, t, X, H)

-- try with start 0, time step 1/steps
xi = 1 
yi = 1
zi = 1
steps = 10
for i from 1 to steps do (
    a = (1/steps)*(i-1);
    b = (1/steps)*i;
    L1 = inputValueTable {t0 => a_R, t1 => b_R, x0 => xi_R, y0 => yi_R, z0 => zi_R};
    V1 = specialize(p, L1); 
    xiN = V1#0; --xi next
    yiN = V1#1; -- yi next
    ziN = V1#2; --zi next

    -- correct with Newton's method m times
    m = 5;
    for j from 1 to m do (
        L2 = inputValueTable {t => b_R, x => xiN_R, y => yiN_R, z => ziN_R};
        V2 = specialize(newtonsOp(hMap({X}, {H})), L2);
        xiN = V2#0;
        yiN = V2#1;
        ziN = V2#2;
    );
    xi = xiN; 
    yi = yiN;
    zi = ziN;
)

<< "(x, y, z): (" << xiN << ", " << yiN << ", " << ziN << ")" << endl;
<< "evaluate F: " << specialize(F, inputValueTable {x => xiN_R, y => yiN_R, z => ziN_R}) << endl;