restart
load "HGates.m2"
declareVariable x
declareVariable y
declareVariable z
declareVariable w

-- Test diff
diff (x, x+y+x+x+z)
diff (x, x*y*x)
diff (x, x*y+x*(x*y))
diff (x, x/(x*x*y))
x/x
diff (x, x/x)
diff (x, detHGate(2, {x, y, z, oneHGate}))
diff (x, detHGate(2, {x, x, x, oneHGate}))
diff (x, detHGate(1, {x}))
diff (x, solveHGate(1, {x}, {x})) -- expect 0
diff (x, solveHGate(1, {x}, {y}))
diff (x, solveHGate(2, {x, y, x, x}, {w, z})) -- expect 4 + 3 + 4 + 4

-- Test DetHGate
detHGate(2, {x, y, z, oneHGate})
detHGate(2, {x, y, z, zeroHGate})
detHGate(2, {x, y, z, w})

-- checking operations work
detHGate(2, {x, y, z, oneHGate}) + detHGate(2, {x, y, oneHGate, zeroHGate})
detHGate(2, {x, y, z, oneHGate}) * detHGate(2, {x, y, oneHGate, zeroHGate})
detHGate(2, {x, y, z, oneHGate}) / detHGate(2, {x, y, oneHGate, zeroHGate})
detHGate(2, {x, y, z, oneHGate}) / 0 -- expect error
oneHGate * detHGate(2, {x, y, oneHGate, zeroHGate})

A = {x, x, x, zeroHGate, y, y, zeroHGate, zeroHGate, z}
detHGate(3, A)
diff (x, detHGate(3, A))

B = {oneHGate, zeroHGate, zeroHGate, zeroHGate, oneHGate, zeroHGate, zeroHGate, zeroHGate, oneHGate}
-- expect 1
detHGate(3, B)

C = {oneHGate, zeroHGate, zeroHGate, zeroHGate, x, zeroHGate, zeroHGate, zeroHGate, oneHGate}
-- expect x
detHGate(3, C)
diff (x, detHGate(3, C))

-- testing times
for n in 5..1000 do (
    D = toList (0..n*n-1) / (i -> if i%3 == 0 then x else (if i%7 == 0 then zeroHGate else oneHGate));
    assert (#D == n*n);
    E = toList (0..n*n-1) / (i -> if i%3 == 0 then x else (if i%7 == 0 then y else z));
    assert (#E == n*n);
    << "Computing detHGate for square matrix size " << n << ": w/ 0s, 1s, and w/o" << endl;
    time detHGate(n, D);
    time detHGate(n, E);
);

-- Test SolveHGate
-- expect expanded form of {1, y}
s1 = solveHGate(2, {x, zeroHGate, zeroHGate, oneHGate}, {x, y})

-- expect some expanded form of {w/x, -zw/x^2 + w/x}
s2 = solveHGate(2, {x, zeroHGate, z, x}, {w, w})

-- doesn't sum element-wise (yet)
s1 + s2
-- examples of operations that need to be written out
s1 + oneHGate
s1 * s2
s1 / s2
detHGate(2, {s1, oneHGate, s2, oneHGate})

-- testing times
for n in 5..1000 do (
    D = toList (0..n*n-1) / (i -> if i%3 == 0 then x else (if i%7 == 0 then zeroHGate else oneHGate));
    assert (#D == n*n);
    E = toList (0..n*n-1) / (i -> if i%3 == 0 then x else (if i%7 == 0 then y else z));
    assert (#E == n*n);
    b = toList (0..n-1) / (i -> if i%4 == 0 then x else (if i%5 == 0 then w else y));
    << "Computing solveHGate for square matrix size " << n << ": w/ 0s, 1s, and w/o" << endl;
    time solveHGate(n, D, b);
    time solveHGate(n, E, b);
);
