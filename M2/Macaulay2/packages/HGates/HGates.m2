HGate = new Type of HashTable
length HGate := g -> error "length(number of outputs) not defined for abstract HGate" -- need to define for each Type that inherits from HGate

InputHGate = new Type of HGate -- "abstract" unit of input  
inputHGate = method()
inputHGate Thing := a -> new InputHGate from {
    Name => a
    }
isConstant InputHGate := a -> (instance(a.Name,Number) or instance(a.Name, RingElement)) 
net InputHGate := g -> net g.Name
length InputHGate := g -> 1 
diff (InputHGate, InputHGate) := (x,y) -> if y === x then oneHGate else zeroHGate
diff (InputHGate, HGate) := (x,g) -> error "diff not defined for abstract HGate"

oneHGate = inputHGate 1
minusOneHGate = inputHGate(-1)
zeroHGate = inputHGate 0

declareVariable = method()
declareVariable Symbol :=  
declareVariable IndexedVariable := g -> (g <- inputHGate g) 
declareVariable InputHGate := g -> g
declareVariable Thing := g -> error "defined only for a Symbol or an IndexedVariable" 

SumHGate = new Type of HGate
net SumHGate := g -> "(" | net first g.Inputs | "+" | net last g.Inputs | ")"
HGate + HGate := (a,b) -> (
    if a===zeroHGate then b else 
    if b===zeroHGate then a else 
    new SumHGate from {
      	Inputs => (a,b)
      	} 
    )
length SumHGate := g -> 1
diff (InputHGate, SumHGate) := (x,g) -> diff(x,first g.Inputs) + diff(x,last g.Inputs)

ProductHGate = new Type of HGate
net ProductHGate := g -> "(" | net first g.Inputs | "*" | net last g.Inputs | ")"
HGate * HGate := (a,b) -> (
    if a===zeroHGate or b===zeroHGate then zeroHGate else 
    if a===oneHGate then b else 
    if b===oneHGate then a else 
    new ProductHGate from {
        Inputs => (a,b)
        } 
    )
length ProductHGate := g -> 1
-*
diff...
*-

DivHGate = new Type of HGate
net DivHGate := g -> "(" | net first g.Inputs | "/" | net last g.Inputs | ")"
HGate / HGate := (a,b) -> (
    if b===zeroHGate then error "division by zero";
    if a===zeroHGate then zeroHGate else
    if b===oneHGate then a else 
    new DivHGate from {
        Inputs => (a,b)
        } 
    )
length DivHGate := g -> 1
-*
diff ...
*-

-- helper method for getting A_{i, j} submatrix
-- assume A is of length n^2 for n > 1
-- i, j in (0, n-1)
-- returns a list of size (n-1)^2 
subHMatrix = method()
subHMatrix(ZZ,ZZ,ZZ,List) := (i, j, n, A) -> (
    --<< "[subHMatrix] Current A: " << A << " and n: " << n << " and j: " << j << endl;
    assert(i >= 0 and i < n);
    assert(j >= 0 and j < n);
    B := drop(A, {n*i, n*i + n - 1}); -- drop i-th row
    for k from 0 to n-2 do ( -- drop j-th column
        B = drop(B, {k*n + j - k, k*n + j - k});
    );
    --<< "[subHMatrix] B: " << B << endl;
    assert(#B == (n-1)^2);
    B
)


DetHGate = new Type of HGate
net DetHGate := g -> (
    n := first g.Inputs;
    A := last g.Inputs;

    -- Laplace/Cofactor expansion along first row
    M := ((0..(n-1)) / 
        (i -> if i%2 == 0 then (
                A#i * detHGate(n-1, subHMatrix(0, i, n, A))
            ) else (
                minusOneHGate * A#i * detHGate(n-1, subHMatrix(0, i, n, A))
            )  
        ));

    d := fold(plus, M);

    net d
    )
detHGate = method()
detHGate(ZZ,List) := (n,A) -> (
    if #A != n^2 then error "data array is not matching the size of the matrix";
    if n == 1 then A#0 else (
    new DetHGate from {
        Inputs => (n, A)        
        }
    ))
length DetHGate := g -> 1
-*
diff...
*-

SolveHGate = new Type of HGate
-- solves for x = A^{-1} b
-- assumes detA != 0
net SolveHGate := g -> (
    -- note: first g.Inputs is A, last g.Inputs is b
    --<< "[net SolveHGate] A: " << first g.Inputs << ", b: " << last g.Inputs << endl;
    n := #last g.Inputs; -- assume length b is n (from solveHGate assertion)
    A := first g.Inputs;
    b := last g.Inputs;
    -- compute det(A)
    detA := detHGate(n, A);
    --<< "[SolveHGate] detA: " << net detA << endl;
    
    -- compute adjugate(A)
    adjA := flatten (toList(0..(n-1)) / (i -> (
        toList(0..(n-1)) / (j -> (
            if (i + j) % 2 == 0 then 
                -- (i, j) transpose of cofactor
                detHGate(n-1, subHMatrix(j, i, n, A)) 
            else (
                -- (i, j) transpose of cofactor 
                minusOneHGate * detHGate(n-1, subHMatrix(j, i, n, A))
        ))
    ))));
    --<< "[SolveHGate] adjA: " << net adjA << endl;
    
    -- compute x = A^{-1} b = adjA * b / detA
    returnX := toList (((0..(n-1)) / 
        (i -> (fold (plus, ((0..(n-1))/ -- i-th row of adjA
            (j -> ( adjA#(i*n + j) * b#j / detA))
    )))))); -- j-th column adjA, j-th row of b

    net returnX
    
    )
solveHGate = method()
solveHGate(ZZ,List,List) := (n,A,b) -> (
    if #A != n^2 then error "`A` data array is not matching the expected size of the matrix";
    if #b != n then error "`b` data array is not matching the expected size of the matrix";
    if n == 1 then "b#0 / A#0" else
    new SolveHGate from {
        Inputs => (A,b)      
        }
    )
length SolveHGate := g -> #last g.Inputs
-*
diff...
*-

end
