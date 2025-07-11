-- from SPLexpressions, for printing
concatenateNets = method()
concatenateNets List := L -> (
    result := net "";
    for a in L do result = result | net a;
    result
    )

isSquare = n -> (
    if n < 0 then return false; 
    s = floor(sqrt(n));        
    s*s == n         
)

-- H version of Gates ---------------------------------------------

HMatrixGate = new Type of HashTable
net HMatrixGate := g -> (
    A := g.Elements; -- List of HMatrixGates
    concatenateNets{"|", A, "|"}
    )
length HMatrixGate := g -> g.Size -- number of HMatrixGates in the matrix

InputValueTable = new Type of HashTable -- table of input values
inputValueTable = method()
inputValueTable List := L -> new InputValueTable from hashTable L
ValueList = new Type of List -- list of values
valueList = method()
valueList List := L -> new ValueList from L
specialize = method() -- specializing InputHGates to values

flatten HMatrixGate := g -> (
    A := g.Elements; 
    n := g.Size; -- total number of entries in the matrix
    if #A == n then return g; -- already flattened
    AFlatten := flatten {A/(e -> flatten e)}; -- flatten the list of HMatrixGates
    hMatrixGate(AFlatten, g.Size) -- return a new HMatrixGate with the flattened list
)
specialize (HMatrixGate, InputValueTable) := (g, L) -> (
    A := g.Elements;
    nestedEvalA := valueList A/(e -> specialize(e, L));
    evalA := flatten nestedEvalA; 
    evalA 
    )
hMatrixGate = method()  
hMatrixGate (List, ZZ) := (A, n) -> (
    if not all(A, (e -> instance(e, HMatrixGate))) then error "data array is not a list of HMatrixGates";
    tempA := flatten {A/(e -> if length e == 1 then e 
                                else (tempe := flatten e;
                                    tempe.Elements
                                ))};
    AFlatten := flatten tempA; -- flatten the list of HMatrixGates
    new HMatrixGate from {
        Elements => AFlatten,
        Size => n
        }
    )

InputHMatrixGate = new Type of HMatrixGate -- "abstract" unit of input
inputHMatrixGate = method()
inputHMatrixGate Thing := a -> new InputHMatrixGate from {
    Name => a
    }
isConstant InputHMatrixGate := a -> (instance(a.Name,Number) or instance(a.Name, RingElement)) 
net InputHMatrixGate := g -> net g.Name
length InputHMatrixGate := g -> 1 
specialize (InputHMatrixGate, InputValueTable) := (g, L) -> valueList {
    if isConstant g then g.Name else 
    if L#?g then L#g else error "value not found for input"
    }
diff (InputHMatrixGate, InputHMatrixGate) := (x,y) -> if y === x then oneHMatrixGate else zeroHMatrixGate
diff (InputHMatrixGate, HMatrixGate) := (x,g) -> (
    A := g.Elements; -- List of HMatrixGates
    diffA := A/(e -> diff(x, e));
    hMatrixGate(diffA, g.Size) -- diff each HMatrixGate in the list
    )

oneHMatrixGate = inputHMatrixGate 1
minusOneHMatrixGate = inputHMatrixGate(-1)
zeroHMatrixGate = inputHMatrixGate 0

declareVariable = method()
declareVariable Symbol :=  -- ???
declareVariable IndexedVariable := g -> (g <- inputHMatrixGate g) 
declareVariable InputHMatrixGate := g -> g
declareVariable Thing := g -> error "defined only for a Symbol or an IndexedVariable" 

SumHMatrixGate = new Type of HMatrixGate
net SumHMatrixGate := g -> "(" | net first g.Inputs | "+" | net last g.Inputs | ")"
HMatrixGate + HMatrixGate := (a,b) -> (
    if length a != 1 or length b != 1 then error "can only sum two HMatrixGates of size 1";
    if a===zeroHMatrixGate then b else 
    if b===zeroHMatrixGate then a else 
    new SumHMatrixGate from {
      	Inputs => (a,b)
      	} 
    )
sumHMatrixGate = method()
sumHMatrixGate(HMatrixGate) := (M) -> (
    if length M != 2 then error "expecting 2 HMatrixGates";
    a := (M.Elements)#0;
    b := (M.Elements)#1;
    if length a != 1 or length b != 1 then error "can only sum two HMatrixGates of size 1";
    if a===zeroHMatrixGate then b else 
    if b===zeroHMatrixGate then a else 
    new SumHMatrixGate from {
      	Inputs => (a,b)
      	} 
    )
length SumHMatrixGate := g -> 1 -- wrong, need to sum the size of the inputs
specialize (SumHMatrixGate, InputValueTable) := (g, L) -> specialize(first g.Inputs, L) + specialize(last g.Inputs, L)
diff (InputHMatrixGate, SumHMatrixGate) := (x,g) -> diff(x,first g.Inputs) + diff(x,last g.Inputs)

ProductHMatrixGate = new Type of HMatrixGate
net ProductHMatrixGate := g -> "(" | net first g.Inputs | "*" | net last g.Inputs | ")"
HMatrixGate * HMatrixGate := (a,b) -> (
    if length a != 1 or length b != 1 then error "can only multiply two HMatrixGates of size 1";
    if a===zeroHMatrixGate or b===zeroHMatrixGate then zeroHMatrixGate else 
    if a===oneHMatrixGate then b else 
    if b===oneHMatrixGate then a else 
    new ProductHMatrixGate from {
        Inputs => (a,b)
        } 
    )
productHMatrixGate = method()
productHMatrixGate(HMatrixGate) := (M) -> (
    if length M != 2 then error "expecting 2 HMatrixGates";
    a := (M.Elements)#0;
    b := (M.Elements)#1;
    if length a != 1 or length b != 1 then error "can only multiply two HMatrixGates of size 1";
    if a===zeroHMatrixGate or b===zeroHMatrixGate then zeroHMatrixGate else 
    if a===oneHMatrixGate then b else 
    if b===oneHMatrixGate then a else 
    new ProductHMatrixGate from {
        Inputs => (a,b)
        } 
    )
length ProductHMatrixGate := g -> 1
specialize (ProductHMatrixGate, InputValueTable) := (g, L) -> valueList { (specialize(first g.Inputs, L))#0 * (specialize(last g.Inputs, L))#0 }
diff (InputHMatrixGate, ProductHMatrixGate) := (x,g) -> (first g.Inputs)*diff(x,last g.Inputs) + (last g.Inputs)*diff(x,first g.Inputs)

DetHMatrixGate = new Type of HMatrixGate
net DetHMatrixGate := g -> (
    M := g.Inputs;

    concatenateNets {"det", net M}

    )
detHMatrixGate = method()
detHMatrixGate(HMatrixGate) := M -> (
    A := M.Elements;
    n := M.Size;
    if not isSquare(n) then error "Error, expecting a square matrix";
    if n == 1 then A#0 else (
        new DetHMatrixGate from {
            Inputs => M        
            }
    ))
length DetHMatrixGate := g -> 1
specialize (DetHMatrixGate, InputValueTable) := (g, L) -> (
    M := g.Inputs;
    n := M.Size; 
    row := floor sqrt n; 
    evalA := specialize(M, L); -- valueList
    squareMatrixList := toList (0..(row-1)) / (i -> (
        toList (0..(row-1)) / (j -> evalA#(i*row + j))
    )); -- convert to a list of lists
    squareMatrix := matrix squareMatrixList; -- convert to a matrix
    valueList {det squareMatrix}
    )

diff (InputHMatrixGate, DetHMatrixGate) := (x,g) -> (
    M := g.Inputs;
    A := M.Elements;
    n := floor(sqrt(M.Size)); 
    returnL := (0..n-1) / (i -> 
        detHMatrixGate(hMatrixGate (toList (0..(n*n-1)) / (j -> 
                if j >= i*n and j < (i+1)*n then diff(x, A#j) else A#j), n*n)));
    fold(plus, returnL)
    )


ElementHMatrixGate = new Type of HMatrixGate
net ElementHMatrixGate := g -> (
    M := first g.Inputs;
    i := last g.Inputs;
    if i < 0 then error "index < 0";

    concatenateNets {M, "[", i, "]"}
    )
length ElementHMatrixGate := g -> 1
specialize (ElementHMatrixGate, InputValueTable) := (g, L) -> (
    M := first g.Inputs;
    i := last g.Inputs;
    evalA := specialize(M, L); -- computationally not great, but preserves blackbox structure
    valueList { evalA#i }
    )
diff (InputHMatrixGate, ElementHMatrixGate) := (x,g) -> (
    diffM := diff(x, first g.Inputs); -- computationally not great, but preserves blackbox structure
    elementHMatrixGate(diffM, last g.Inputs) 
    )
elementHMatrixGate = method()
elementHMatrixGate (HMatrixGate, ZZ) := (M, i) -> (
    if i < 0 then error "index < 0";
    innerElement := new ElementHMatrixGate from {
        Inputs => (M, i)
        };
    new HMatrixGate from {
        Elements => {innerElement},
        Size => 1
    }
    )

BigSumHMatrixGate = new Type of HMatrixGate
net BigSumHMatrixGate := g -> (
    M := first g.Inputs; 
    N := last g.Inputs; 

    "(" | net M | "+" | net N | ")"
    )
bigSumHMatrixGate = method()
bigSumHMatrixGate(HMatrixGate, HMatrixGate) := (M, N) -> (
    if length M != length N then error "M and N must have the same length";
    innerElement := new BigSumHMatrixGate from {
            Inputs => (M, N)
            };
    new HMatrixGate from {
        Elements => {innerElement},
        Size => 1
        }
    )
length BigSumHMatrixGate := g -> length first g.Inputs -- assumes both inputs have the same length
specialize (BigSumHMatrixGate, InputValueTable) := (g, L) -> (
    M := first g.Inputs; 
    N := last g.Inputs; 
    evalA := specialize (M, L);
    evalB := specialize (N, L);
    n := #evalA;
    valueList toList (0..(n-1))/(i -> evalA#i + evalB#i) 
    )
diff (InputHMatrixGate, BigSumHMatrixGate) := (x,g) -> (
    M := first g.Inputs; 
    N := last g.Inputs;
    bigSumHMatrixGate(diff (x, M), diff(x, N))
    )

BigProductHMatrixGate = new Type of HMatrixGate
net BigProductHMatrixGate := g -> (
    M := (g.Inputs)#1; 
    N := (g.Inputs)#2; 
    "(" | net M | "*" | net N | ")"
    )
bigProductHMatrixGate = method()
-- I is a list of 3 integers denoting the size of M and N respectively
bigProductHMatrixGate(List, HMatrixGate, HMatrixGate) := (I, M, N) -> (
    innerElement := new BigProductHMatrixGate from {
            Inputs => (I, M, N)
            };
    new HMatrixGate from {
        Elements => {innerElement},
        Size => 1
        }
    )
length BigProductHMatrixGate := g -> (
    I := (g.Inputs)#0;
    n := I#0;
    m := I#2;
    n*m
)
specialize (BigProductHMatrixGate, InputValueTable) := (g, L) -> (
    M := (g.Inputs)#1; 
    N := (g.Inputs)#2; 
    I := (g.Inputs)#0;
    n := I#0;
    k := I#1;
    m := I#2;
    evalA := specialize (M, L);
    evalB := specialize (N, L);
    listMatrixA := toList (0..(n-1)) / (i -> (
        toList (0..(k-1)) / (j -> evalA#(i*k + j))
    )); -- convert to a list of lists
    matrixA := matrix listMatrixA; 
    listMatrixB := toList (0..(k-1)) / (i -> (
        toList (0..(m-1)) / (j -> evalB#(i*m + j))
    )); -- convert to a list of lists
    matrixB := matrix listMatrixB;
    resultAxB := valueList flatten entries (matrixA * matrixB);
    resultAxB
    )
diff (InputHMatrixGate, BigProductHMatrixGate) := (x,g) -> (
    M := (g.Inputs)#1; 
    N := (g.Inputs)#2; 
    I := (g.Inputs)#0;

    bigSumHMatrixGate(
        bigProductHMatrixGate(I, M, diff(x, N)),
        bigProductHMatrixGate(I, diff(x, M), N)
        ))

SolveHMatrixGate = new Type of HMatrixGate
-- solves for x = A^{-1} b
-- assumes detA != 0
net SolveHMatrixGate := g -> (
    M := first g.Inputs; 
    N := last g.Inputs; 

    -- see overleaf for explanation
    concatenateNets {"solve(", M, ", ", N, ")"}
    
    )
solveHMatrixGate = method()
solveHMatrixGate(HMatrixGate, HMatrixGate) := (M, N) -> (
    A := M.Elements;
    b := N.Elements;
    n := length N; -- assume M is a square matrix

    if #A != n^2 then error "A is not matching the expected size of the matrix";
    if #b != n then error "b is not matching the expected size of the matrix";
    innerElement := new SolveHMatrixGate from {
        Inputs => (M, N)    
        };
    new HMatrixGate from {
        Elements => {innerElement},
        Size => 1
    }
    )
length SolveHMatrixGate := g -> (
    N := last g.Inputs; 
    length N
)
specialize (SolveHMatrixGate, InputValueTable) := (g, L) -> (
    M := first g.Inputs;
    N := last g.Inputs;
    n := length N;
    evalA := specialize (M, L);
    evalB := specialize (N, L);
    listMatrixA := toList (0..(n-1)) / (i -> (
        toList (0..(n-1)) / (j -> evalA#(i*n + j))
    )); -- convert to a list of lists
    matrixA := matrix listMatrixA; 
    inverseMatrixA := inverse matrixA;
    listMatrixB := toList (0..(n-1)) / (i ->
        {evalB#i}
    ); -- convert to a list of lists
    matrixB := matrix listMatrixB;
    valueList flatten entries (inverseMatrixA * matrixB)
    )
diff (InputHMatrixGate, SolveHMatrixGate) := (x,g) -> (
    M := first g.Inputs; 
    N := last g.Inputs; 
    n := length N; -- assume length b is n (from solveHMatrixGate assertion)
    A := M.Elements;
    b := N.Elements;

    -- base case, 1x1 matrix, needs a divide gate
    doubleListA := toList(0..(n-1)) / (i -> (
        toList(0..(n-1)) / (j -> A#(i*n + j))
    ));
    -- SolveHMatrixGate for A and each partial diff column of A
    -- list of length n with SolveHMatrixGate entries
    partialOfA := toList (0..(n-1))/(i -> (
                solveHMatrixGate(M, hMatrixGate(toList(0..(n-1)) / (j -> diff(x, A#(i*n+j))), n))));
    flatListForMatrix := flatten (toList (0..(n-1)) / (i -> (
        toList(0..(n-1)) / (j -> elementHMatrixGate(partialOfA#j, i) 
        ))
    ));
    colMatrixHMatrixGates := hMatrixGate(flatListForMatrix, n*n);

    -- convert list to vector
    partialOfb := toList b / (e -> diff(x, e));
    partialN := hMatrixGate(partialOfb, n);

    -- see overleaf for explanation
    bigSumHMatrixGate(bigProductHMatrixGate({n, n, 1}, colMatrixHMatrixGates, solveHMatrixGate(M, N)), solveHMatrixGate(M, partialN))
    
    )


-- H version of Straight-line Programs ---------------------------------------------

HSLP = new Type of HashTable
length HSLP := P -> (
    #P.Gates
    )
-- note: "size" is a protected global variable, so using sizeSLP
sizeSLP = method()
sizeSLP HSLP := P -> (
    Graph := P.Graph;
    listOfGateSizes := G / (i -> length Graph#i);
    fold(plus, listOfGateSizes)
    )

-- returns specialization of the outputs of the HSLP
specialize (HSLP, InputValueTable) := (P, L) -> (
    Graph := P.Graph;
    -- start
    for i in (sort keys Graph) do (
        << "current gate [" << i << "]: " << Graph#i << endl;
        << "specialize gate [" << i << "]: " << specialize (Graph#i, L) << endl;
    );

    -- end
    evalGraph := new HashTable from (sort keys Graph) / (i -> i => specialize (Graph#i, L));
    << "finished evalGraph " << evalGraph << endl;
    O / (i -> evalGraph#i)
    )

hSLP = method()
-- TODO: probably H is a better variable than P, change this
hSLP(HashTable, List, List) := (P, I, O) -> (
        -- check inputs in P
        I / (i -> assert(P#?i and (P#i)#0 === inputHMatrixGate and #((P#i)#1) == 1 ));

        -- check outputs in P
        O / (i -> assert(P#?i));

        -- check acyclic graph 
        (keys P) / (i -> assert(any(I, j -> j == i) or i > max (P#i)#1));

        -- build list of gate indices
        G := {};
        (keys P) / (i -> if not I#?i then G = append(G, i));

        -- construct HashTable of HMatrixGates
        exP := new MutableHashTable;
        (sort keys P) / (i -> if any(I, j -> j == i) then (
                                var := ((P#i)#1)#0;
                                exP#i = var
                         ) else   (
                                M := hMatrixGate((P#i)#1 / (j -> exP#j), #((P#i)#1));
                                func := (P#i)#0;
                                gate := func M;
                                exP#i = gate
                         ));

        new HSLP from {
            Graph => exP,
            Inputs => I,
            Gates => G,
            Outputs => O
        }
    )


end
