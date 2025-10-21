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

--test
X0 = hMatrixGate({inputHGate 1., inputHGate 1.}, 2, 1)
initVars = {inputHGate 0., inputHGate 1., X0}
H = ((oneHGate - t)*G) + ((inputHGate 0.5)*t*F); -- \gamma arbitrarily selected
M = hMap({t, X}, {H});
time P = predictorRK4HMatrixGate(M, initVars)

X1literal = predictorCorrector(MF, MG, Gsol, d)

<< "Solutions found X': " << X1literal << endl;
evalF = toList (specialize(F, inputValueTable {x => X1literal#0, y => X1literal#1}))
<< "Evaluation of target system F(X'): " << evalF << endl;
--<< "Difference with known solution (2, -2) under 2-norm: " << sqrt fold(plus, (X1literal - {2, -2})/(i -> i*i)) << endl;
<< "||F(X')||_2: " << sqrt fold(plus, (evalF)/(i -> i*i)) << endl;


-- Conic Problem 1
-- Given 5 points in R^2
-- with 2 complete points and 3 points based on two indeterminants
-- return the conic passing through all points has a focus at the origin
-- and the values for the indeterminants

restart
needs "../HGates.m2"
R = RR_53
declareVariable \ {A, B, C, D, E, x, y, t}

-- 1. Define given points
-- answer (x,y) = (1,3) doesn't work
--(x3, y3) = ((inputHGate 9) * x, y - threeHGate) 
--(x4, y4) = (fourHGate, y)
--(x5, y5) = (x, inputHGate (12/5)) -- y = 1
--(x1, y1) = (twoHGate, inputHGate (3*sqrt(21)/5)) -- x = sqrt3
--(x2, y2) = (threeHGate, inputHGate (6*sqrt(6)/5))

-- answer (x,y) = (0, 1) works!
--(x1, y1) = (inputHGate (-2), zeroHGate)
--(x2, y2) = (twoHGate, zeroHGate)
--(x3, y3) = (x, twoHGate)
--(x4, y4) = (x, minusOneHGate * twoHGate * y)
--(x5, y5) = (inputHGate sqrt(3), y)

-- answer (x,y) = (2, 3) not even close
(x5, y5) = (zeroHGate, y)
(x3, y3) = (x-twoHGate, inputHGate (-3/7)*y)
(x1, y1) = (inputHGate (2*sqrt(3)), twoHGate + inputHGate (5*sqrt(3/7)))
(x2, y2) = (inputHGate (2*sqrt(3)) + inputHGate (15/sqrt(13)), twoHGate)
(x4, y4) = (x * inputHGate sqrt(3) - inputHGate (15/sqrt(13)), twoHGate)

-- 2. Define HMap of F = (f1, ... f7)
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

-- 3. Pick a starting system G and define HMap of G
-- confirm that jacobian of G at Gsol is nonsingular

-- let G be a unit circle centered at the origin
-- solution is known to be (x,y) = -sqrt(2)/2), -sqrt(2)/2)
(x1g, y1g) = (inputHGate 0., inputHGate 1.)
(x2g, y2g) = (inputHGate (-1.), inputHGate 0)
(x3g, y3g) = (x, y)
(x4g, y4g) = (x, inputHGate (sqrt(2)/2))
(x5g, y5g) = (inputHGate (sqrt(2)/2), y)

g1 = f0(x1g, y1g)
g2 = f0(x2g, y2g)
g3 = f0(x3g, y3g)
g4 = f0(x4g, y4g)
g5 = f0(x5g, y5g)
g6 = A - C - (inputHGate 0.25 * D * D) + (inputHGate 0.25 * E * E) 
g7 = (inputHGate 0.5 * B) - (inputHGate 0.25 * D * E)

G = hMatrixGate({g1, g2, g3, g4, g5, g6, g7}, 7, 1)
MG = hMap({X}, {G})
Gsol = {-1., 0., -1., 0., 0., -sqrt(2)/2, -sqrt(2)/2}
d = 0.01

-- 4. Use homtopy continutation to get a solution
X1literal = predictorCorrector(MF, MG, Gsol, d)

-- 5. Verify conic contains all five points
Xvars = X.Elements;
L = inputValueTable (toList (0..n-1)/(i -> Xvars#i => X1literal#i))
-- expect close to zero
specialize(f1, L)
specialize(f2, L)
specialize(f3, L)
specialize(f4, L)
specialize(f5, L)

-- SCRATCH

-- 7. check a focus of the resulting conic is the origin
-- (get standard form foci, work backwards to confirm is origin or not)
Q = matrix{{X1literal#0, X1literal#1 * 0.5}, {X1literal#1 * 0.5, X1literal#2}}
(e, P) = eigenvectors(Q, Hermitian => true)
P = -1*P*matrix{{0, 1}, {1, 0}}
DEprime = (transpose P)*(matrix{{X1literal#3}, {X1literal#4}})
Dprime = ((entries DEprime)#0)#0 -- D'
Eprime = ((entries DEprime)#1)#0 -- E'
eval1 = -e#1 -- first eigenvalue
eval2 = -e#0 -- second eigenvalue
K = (Dprime^2)/(4*eval1) + (Eprime^2)/(4*eval2) + 1
Delta = (X1literal#1)^2 - 4*(X1literal#0)*(X1literal#2) -- B^2 - 4AC
(h, k) = (Dprime/(2*eval1), Eprime/(2*eval2)) -- center

if (Delta < 0) then ( -- conic is an ellipse
  if (eval1 < 0) then (
    << "Oh no our ellipse is imaginery" << endl;
  ) else (
    a = max(sqrt(K/eval1), sqrt(K/eval2));
    b = min(sqrt(K/eval1), sqrt(K/eval2));
    c = sqrt(a^2 - b^2);
    << "Columns of matrix are Foci of Ellipse: " << endl;
    (F1, F2) = (P*matrix{{c+h}, {k}}, P*matrix{{-c+h}, {k}}) -- literal foci
  )
) else (
  << "This should have been an ellipse" << endl;
)
