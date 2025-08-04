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
length HMatrixGate := g -> g.Rows*g.Cols -- number of HMatrixGates in the matrix

InputValueTable = new Type of HashTable -- table of input values
inputValueTable = method()
inputValueTable List := L -> new InputValueTable from hashTable L
inputValueTable HashTable := H -> new InputValueTable from H
ValueList = new Type of List -- list of values
valueList = method()
valueList List := L -> new ValueList from L
specialize = method() -- specializing InputHGates to values

-- flatten HMatrixGate into list of size 1 HMatrixGates
flatten HMatrixGate := g -> (
    A := g.Elements; 
    n := length g; -- total number of entries in the matrix
    if #A == n then return g; -- already flattened
    AFlatten := flatten {A/(e -> flatten e)}; -- flatten the list of HMatrixGates
    hMatrixGate(AFlatten, n) -- return a new HMatrixGate with the flattened list
)
specialize (HMatrixGate, InputValueTable) := (g, L) -> (
    A := g.Elements;
    nestedEvalA := valueList A/(e -> specialize(e, L));
    evalA := flatten nestedEvalA; 
    evalA 
    )
hMatrixGate = method()  
hMatrixGate (List, ZZ, ZZ) := (A, r, c) -> (
    if not all(A, (e -> instance(e, HMatrixGate))) then error "input is not a list of HMatrixGates";
    tempA := flatten {A/(e -> if length e == 1 then e 
                                else (tempe := flatten e;
                                    tempe.Elements
                                ))};
    AFlatten := flatten tempA; -- flatten the list of HMatrixGates
    new HMatrixGate from {
        Elements => AFlatten,
        Rows => r,
        Cols => c
        }
    )

InputHMatrixGate = new Type of HMatrixGate -- "abstract" unit of input
inputHMatrixGate = method()
inputHMatrixGate Thing := a -> new InputHMatrixGate from {
    Name => a,
    Rows => 1,
    Cols => 1
    }
isConstant InputHMatrixGate := a -> (instance(a.Name,Number) or instance(a.Name, RingElement)) 
net InputHMatrixGate := g -> "'" | net g.Name | "'"
length InputHMatrixGate := g -> 1 
specialize (InputHMatrixGate, InputValueTable) := (g, L) -> valueList {
    if isConstant g then g.Name else 
    if L#?g then L#g else error "value not found for input"
    }
diff (InputHMatrixGate, InputHMatrixGate) := (x,y) -> if y === x then oneHMatrixGate else zeroHMatrixGate
diff (InputHMatrixGate, HMatrixGate) := (x,g) -> (
    A := g.Elements; -- List of HMatrixGates
    diffA := A/(e -> diff(x, e));
    hMatrixGate(diffA, g.Rows, g.Cols) -- diff each HMatrixGate in the list
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
      	Inputs => (a,b),
        Rows => 1,
        Cols => 1
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
      	Inputs => (a,b),
        Rows => 1,
        Cols => 1
      	} 
    )
length SumHMatrixGate := g -> 1 
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
        Inputs => (a,b),
        Rows => 1,
        Cols => 1
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
        Inputs => (a,b),
        Rows => 1,
        Cols => 1
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
    r := M.Rows;
    c := M.Cols;
    if r != c then error "Error, expecting a square matrix";
    if r == 1 then A#0 else (
        new DetHMatrixGate from {
            Inputs => M        
            }
    ))
length DetHMatrixGate := g -> 1
specialize (DetHMatrixGate, InputValueTable) := (g, L) -> (
    M := g.Inputs;
    row := M.Cols; 
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
    n := M.Rows; 
    returnL := (0..n-1) / (i -> 
        detHMatrixGate(hMatrixGate (toList (0..(n*n-1)) / (j -> 
                if j >= i*n and j < (i+1)*n then diff(x, A#j) else A#j), n, n)));
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
-- assumes single index to index an element
elementHMatrixGate (HMatrixGate, ZZ) := (M, i) -> (
    if i < 0 then error "index < 0";
    innerElement := new ElementHMatrixGate from {
        Inputs => (M, i)
        };
    new HMatrixGate from {
        Elements => {innerElement},
        Rows => 1,
        Cols => 1
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
    if M.Rows != N.Rows or M.Cols != N.Cols then error "M and N must have same dimensions";
    new BigSumHMatrixGate from {
            Inputs => (M, N),
            Rows => M.Rows,
            Cols => M.Cols
            }
    )
length BigSumHMatrixGate := g -> g.Rows * g.Cols
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
bigProductHMatrixGate(HMatrixGate, HMatrixGate) := (M, N) -> (
    if M.Cols != N.Rows then error "M.Cols must equal N.Rows";
    new BigProductHMatrixGate from {
            Inputs => (M, N),
            Rows => M.Rows,
            Cols => N.Cols
            }
    )
length BigProductHMatrixGate := g -> (
    g.Rows * g.Cols
    )
specialize (BigProductHMatrixGate, InputValueTable) := (g, L) -> (
    M := first g.Inputs;
    N := last g.Inputs; 
    n := M.Rows;
    k := M.Cols;
    m := N.Cols;
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
    M := first g.Inputs; 
    N := last g.Inputs; 

    bigSumHMatrixGate(
        bigProductHMatrixGate(M, diff(x, N)),
        bigProductHMatrixGate(diff(x, M), N)
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
    n := length N; 

    if M.Rows != n or M.Cols != n then error "A is not matching the expected size of the matrix";
    if N.Rows != n then error "b is not matching the expected size of the matrix";
    new SolveHMatrixGate from {
        Inputs => (M, N), 
        Rows => n,
        Cols => 1
        }
    )
length SolveHMatrixGate := g -> (
    g.Rows
)
specialize (SolveHMatrixGate, InputValueTable) := (g, L) -> (
    M := first g.Inputs;
    N := last g.Inputs;
    n := length g;
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
    n := length g; 
    A := M.Elements;
    b := N.Elements;

    -- base case, 1x1 matrix, needs a divide gate
    doubleListA := toList(0..(n-1)) / (i -> (
        toList(0..(n-1)) / (j -> A#(i*n + j))
    ));
    -- SolveHMatrixGate for A and each partial diff column of A
    -- list of length n with SolveHMatrixGate entries
    partialOfA := toList (0..(n-1))/(i -> (
                solveHMatrixGate(M, hMatrixGate(toList(0..(n-1)) / (j -> diff(x, A#(i*n+j))), n, 1))));
    flatListForMatrix := flatten (toList (0..(n-1)) / (i -> (
        toList(0..(n-1)) / (j -> elementHMatrixGate(partialOfA#j, i) 
        ))
    ));
    colMatrixHMatrixGates := hMatrixGate(flatListForMatrix, n, n);

    -- convert list to vector
    partialOfb := toList b / (e -> diff(x, e));
    partialN := hMatrixGate(partialOfb, n, 1);

    -- see overleaf for explanation
    bigSumHMatrixGate(bigProductHMatrixGate(colMatrixHMatrixGates, solveHMatrixGate(M, N)), solveHMatrixGate(M, partialN))
    
    )


-- H version of Straight-line Programs ---------------------------------------------

HSLP = new Type of HashTable

-- printing functions for SLP
PrintIndices = new Type of MutableHashTable
newPrintIndices = assignmentSymbol -> (p := new PrintIndices; p#"assignmentSymbol"=assignmentSymbol; p#"#consts"=p#"#vars"=p#"#lines"=0; p#"gates" = new MutableHashTable; p)

hMatrixGateType = method() -- return string of specific type of HMatrixGate 
hMatrixGateType (HMatrixGate) := g -> (
    if instance(g, InputHMatrixGate) then "InputHMatrixGate"
    else if instance(g, SumHMatrixGate) then "SumHMatrixGate"
    else if instance(g, ProductHMatrixGate) then "ProductHMatrixGate"
    else if instance(g, DetHMatrixGate) then "DetHMatrixGate"
    else if instance(g, SolveHMatrixGate) then "SolveHMatrixGate"
    else if instance(g, ElementHMatrixGate) then "ElementHMatrixGate"
    else if instance(g, BigSumHMatrixGate) then "BigSumHMatrixGate"
    else if instance(g, BigProductHMatrixGate) then "BigProductHMatrixGate"
    else "HMatrixGate" -- default case
    )

printSLP = method()
printSLP (List, List) := (I, O) -> (
    if not all(I, (e -> instance(e, HMatrixGate))) then error "Error, I is not a list of HMatrixGates";
    if not all(O, (e -> instance(e, HMatrixGate))) then error "Error, O is not a list of HMatrixGates";
    p := newPrintIndices " = ";
    O / (g -> printHMatrixGate(g, p));
    unsortedLines := values p#"gates" / (l -> l#0 | p#"assignmentSymbol" | l#1 );
    sort unsortedLines / (slpLine -> << slpLine << endl);
    O / (g -> << "OUTPUT: " << ((p#"gates")#g)#0 << p#"assignmentSymbol" << ((p#"gates")#g)#1  << endl);
    )

printHMatrixGate = method()
printHMatrixGate (InputHMatrixGate, PrintIndices) := (g,p) -> if (p#"gates")#?g then (p#"gates")#g else (
    if isConstant g then (
	    (p#"gates")#g = {"C"|toString p#"#consts", net g};
    	p#"#consts" = p#"#consts" + 1;
	) else (
	    (p#"gates")#g = {"I"|toString p#"#vars", net g};
    	p#"#vars" = p#"#vars" + 1;
	);
    (p#"gates")#g
    )



printHMatrixGate (SumHMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    a := g.Inputs#0;
    b := g.Inputs#1;
    val := (printHMatrixGate(a,p))#0 | "+" | (printHMatrixGate(b,p))#0;
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHMatrixGate (ProductHMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    a := g.Inputs#0;
    b := g.Inputs#1;
    val := (printHMatrixGate(a,p))#0 | "*" | (printHMatrixGate(b,p))#0;
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHMatrixGate (DetHMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    m := g.Inputs;
    val := "det(" | (printHMatrixGate(m,p))#0 | ")";
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHMatrixGate (SolveHMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    A := g.Inputs#0;
    b := g.Inputs#1;
    val := "solve{" | (printHMatrixGate(A,p))#0 | ", " | (printHMatrixGate(b,p))#0 | "}";
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))



printHMatrixGate (BigSumHMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    M := g.Inputs#0;
    N := g.Inputs#1;
    val := "matrixSum(" | (printHMatrixGate(M,p))#0 | ", " | (printHMatrixGate(N,p))#0 | ")";
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHMatrixGate (BigProductHMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    M := g.Inputs#0;
    N := g.Inputs#1;

    val := "matrixProduct(" | (printHMatrixGate(M,p))#0 | ", " | (printHMatrixGate(N,p))#0 | ")";
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHMatrixGate (ElementHMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    M := g.Inputs#0;
    i := g.Inputs#1;

    c := inputHMatrixGate i;

    if not (p#"gates")#?c then (
        (p#"gates")#c = {"C"|toString p#"#consts", net c};
        p#"#consts" = p#"#consts" + 1;  

    );

    val := (printHMatrixGate(M,p))#0 | "[" | ((p#"gates")#c)#0 | "]";
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHMatrixGate (HMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (

    a := g.Elements;

    listLines := a / (h -> (printHMatrixGate(h,p))#0);

    val := "matrix" | toString listLines | " (" | toString g.Rows | ", " | toString g.Cols | ")";

    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

end
