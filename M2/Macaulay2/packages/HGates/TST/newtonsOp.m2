-- Subsection: Problems Using Newton's Method
-- Subsubsection: Univariate Problem
-- Problem 1
restart
needs "../HGates.m2"
declareVariable \ {x, x0, x1}

-- 1. set up problem, define Newton's Operator
f_1 = x*x +(inputHGate 2)*x + (inputHGate 5)
f_2 = f_1 - (inputHGate 8) -- f_1 - target value
X = hMatrixGate({x}, 1, 1)
F = hMatrixGate({f_2}, 1, 1)
g_1 = hMap({X}, {F})
g_2 = newtonsOp(g_1)

-- 2. pick initial value
R = RR_53
x0 = 0_R
L = inputValueTable {x => x0}
-- confirm f_2'(x) \neq 0
df_2 = diff (x, f_2)
(specialize(df_2, L))#0 != 0

-- 3. check one application of Newton's Operator
-- expect 1.5
x1 = (specialize (g_2, L))#0;
(x1 == (3/2)_R)

-- 4. iterate over x_k until we find one satisfying
-- |x_k - x_{k-1}| < 0.1
track = 0 -- track iterations
-- QUESTION: should abs(x0-x1) be written from HGates.m2
while (abs(x0-x1) >= 0.1) do (
    x0 = x1;
    L = inputValueTable {x => x0};
    assert ((specialize(df_2, L))#0 != 0);
    x1 = (specialize (g_2, L))#0;
    track = track + 1
)

<< "Result: " << x1 << ", found after " << track << " iterations" << endl;
x0 = x1;
L = inputValueTable {x => x0};
ys = (specialize (f_1, L))#0;
<< "Evaluation of f_1: " << ys << ", error: " << abs(ys-8) << endl;

-- Subsection: Problems Using Newton's Method
-- Subsubsection: Multivariate Problem
-- Problem 2
restart
needs "../HGates.m2"
declareVariable \ {x, y, z, x0, x1, y0, y1, z0, z1}

-- 1. set up problem, define Newton's Operator
f_1 = x*x*y + oneHGate
f_2 = x*z + (twoHGate*y)
f_3 = z*z + threeHGate
X = hMatrixGate({x,y, z}, 3, 1)
F_1 = hMatrixGate({f_1, f_2, f_3}, 3, 1)
Yt = hMatrixGate({oneHGate, twoHGate, threeHGate}, 3, 1) -- target value
F_2 = F_1 - Yt -- F_1 - target value
G_1 = hMap({X}, {F_2})

-- 2. pick initial value
R = RR_53
X0 = {1_R, 1_R, 1_R}

X1 = newtonsMethod(G_1, X0)

<< "Result: " << X1 << ", found after " << track << " iterations" << endl;
X0 = X1;
L = inputValueTable {x => X0#0, y => X0#1, z => X0#2};
Ys = specialize(F_1, L)
<< "Evaluation of F_1: " << Ys << ", error: " << sqrt fold(plus, ({1, 2, 3} - Ys)/(i -> i*i)) << endl;


-- old code for Problem 2
G_2 = newtonsOp(G_1)
L = inputValueTable {x => X0#0, y => X0#1, z => X0#2}
-- confirm det jac F_2’(1) \neq 0
dF_2 = jacobian (X, F_2)
detdF_2 = detHGate dF_2
assert((specialize(detdF_2, L))#0 != 0)

-- 3. check one application of Newton's Operator
X1 = specialize(G_2, L)
assert(X1 == {0.5, 1, 0.5})

-- 4. iterate over X_k until we find one satisfying
-- ||X_k - X_{k-1}||_2 < 0.1
track = 0 -- track iterations
while (sqrt fold(plus, (X1 - X0)/(i -> i*i)) >= 0.1) do (
    X0 = X1;
    L = inputValueTable {x => X0#0, y => X0#1, z => X0#2};
    assert((specialize(detdF_2, L))#0 != 0);
    X1 = specialize(G_2, L);
    track = track + 1
)

<< "Result: " << X1 << ", found after " << track << " iterations" << endl;
X0 = X1;
L = inputValueTable {x => X0#0, y => X0#1, z => X0#2};
Ys = specialize(F_1, L)
<< "Evaluation of F_1: " << Ys << ", error: " << sqrt fold(plus, ({1, 2, 3} - Ys)/(i -> i*i)) << endl;


-- Subsection: Problems Using Newton's Method
-- Subsubsection: Homotopy Continuation Problem
-- Problem 3
restart
needs "../HGates.m2"
declareVariable \ {x, y, t}

-- 1. set up problem, define Newton's Operator
f_1 = x*x - (fiveHGate*x) + sixHGate -- roots: 2, 3
f_2 = y*y*y - (threeHGate*y*y) - (fourHGate*y) + twelveHGate -- roots: 2, -2, 3
g_1 = x*x - oneHGate
g_2 = y*y*y - oneHGate

X = hMatrixGate({x,y}, 2, 1)
F = hMatrixGate({f_1, f_2}, 2, 1)
G = hMatrixGate({g_1, g_2}, 2, 1)
H = ((oneHGate - t)*G) + ((inputHGate 0.528)*t*F) -- \gamma based on Frobenius norm

Yt = hMatrixGate({inputHGate 0.1, inputHGate 0.1}, 2, 1) -- target value
t0 = inputHGate 0.2
Ht0 = subGate(t, t0, H)
Hsing = Ht0 - Yt
M_1 = hMap({X}, {Hsing}) -- M for Map
M_2 = newtonsOp(M_1)

-- 2. pick initial value
R = RR_53
X0 = {1.1_R, 1.1_R}

L = inputValueTable {x => X0#0, y => X0#1}
-- confirm det jac Ht0'(X0) \neq 0
dHsing = jacobian (X, Hsing) 
detdHsing = detHGate dHsing
assert((specialize(detdHsing, L))#0 != 0)

-- 3. check one application of Newton's Operator
-- expect ??
X1 = specialize(M_2, L)
--assert(X1 == ??)

-- 4. iterate over X_k until we find one satisfying
-- ||X_k - X_{k-1}||_2 < 0.1
track = 0 -- track iterations
xerror = sqrt fold(plus, (X1 - X0)/(i -> i*i));
while (xerror >= 0.1) do (
    X0 = X1;
    L = inputValueTable {x => X0#0, y => X0#1};
    assert((specialize(detdHsing, L))#0 != 0);
    X1 = specialize(M_2, L);
    xerror = sqrt fold(plus, (X1 - X0)/(i -> i*i));
    track = track + 1
)

<< "Result: " << X1 << ", found after " << track << " iterations" << endl;
X0 = X1;
L = inputValueTable {x => X0#0, y => X0#1};
Ys = specialize(Ht0, L)
<< "Evaluation of Ht0: " << Ys << ", error: " << sqrt fold(plus, ({0.1, 0.1} - Ys)/(i -> i*i)) << endl;

