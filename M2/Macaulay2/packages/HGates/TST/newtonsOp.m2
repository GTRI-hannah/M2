restart
needs "../HGates.m2"
declareVariable \ {x, x0, x1}

-- 1. set up problem, define Newton's Operator
f_1 = x*x +(inputHGate 2)*x + (inputHGate 5)
f_2 = f_1 - (inputHGate 8) 
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
x1 = (specialize ((g_2).OutputGates#0, L))#0;
(x1 == (3/2)_R)

-- 4. iterate over x_k until we find one satisfying
-- |x_k - x_{k-1}| < 0.1
track = 0
while (abs(x0-x1) >= 0.1) do (
    x0 = x1;
    L = inputValueTable {x => x0};
    assert ((specialize(df_2, L))#0 != 0);
    x1 = (specialize ((g_2).OutputGates#0, L))#0;
    track = track + 1
)

<< "Result: " << x1 << ", found after " << track << " iterations" << endl;