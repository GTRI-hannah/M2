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

-- H version of Gates ------------------------------------------------------
HGate = new Type of HashTable
-- everything is an HGate
-- however there is not an HGate constructor or specific methods
-- including net, diff, specialize, length, etc.
-- all HGate objects are created by the methods below
HMatrixGate = new Type of HGate

InputValueTable = new Type of HashTable -- table of input values
inputValueTable = method()
inputValueTable List := L -> new InputValueTable from hashTable L
inputValueTable HashTable := H -> new InputValueTable from H
ValueList = new Type of List -- list of values
valueList = method()
valueList List := L -> new ValueList from L
specialize = method() -- specializing InputHGates to values

-- the following are HGates satisfying the form: \F^m -> \F ----------------
InputHGate = new Type of HGate -- "abstract" unit of input
inputHGate = method()
inputHGate Thing := a -> new InputHGate from {
    Name => a
    }
isConstant InputHGate := a -> (instance(a.Name,Number) or instance(a.Name, RingElement)) 
net InputHGate := g -> net g.Name
length InputHGate := g -> 1 
specialize (InputHGate, InputValueTable) := (g, L) -> valueList {
    if isConstant g then g.Name else 
    if L#?g then L#g else error "value not found for input"
    }
diff (InputHGate, InputHGate) := (x,y) -> if y === x then oneHGate else zeroHGate

oneHGate = inputHGate 1
minusOneHGate = inputHGate(-1)
zeroHGate = inputHGate 0

declareVariable = method()
declareVariable Symbol :=  -- ???
declareVariable IndexedVariable := g -> (g <- inputHGate g) 
declareVariable InputHGate := g -> g
declareVariable Thing := g -> error "defined only for a Symbol or an IndexedVariable" 

SumHGate = new Type of HGate
net SumHGate := g -> "(" | net first g.Inputs | "+" | net last g.Inputs | ")"
HGate + HGate := (g,h) -> (
    if (instance(g, HMatrixGate) or instance(h, HMatrixGate)) then error "add HMatrixGates using sumHMatrixGate";
    if g===zeroHGate then h else 
    if h===zeroHGate then g else 
    new SumHGate from {
      	Inputs => (g,h)
      	} 
    )
length SumHGate := g -> 1 
specialize (SumHGate, InputValueTable) := (g, L) -> specialize(first g.Inputs, L) + specialize(last g.Inputs, L)
diff (InputHGate, SumHGate) := (x,g) -> diff(x,first g.Inputs) + diff(x,last g.Inputs)

-- G - H = G + (-1)*H
HGate - HGate := (g,h) -> (
    if (instance(g, HMatrixGate) and not instance(h, HMatrixGate)) then error "need to subtract same type";
    if (instance(h, HMatrixGate) and not instance(g, HMatrixGate)) then error "need to subtract same type";
    if (instance(g, HMatrixGate) and instance(h, HMatrixGate)) then 
        if g.Rows != h.Rows or g.Cols != h.Cols then
            error "need to subtract same dimension"
        else sumHMatrixGate(g, scalarProductHMatrixGate(minusOneHGate, h))
    else 
        g + (minusOneHGate * h)
    )

ProductHGate = new Type of HGate
net ProductHGate := g -> "(" | net first g.Inputs | "*" | net last g.Inputs | ")"
HGate * HGate := (g,h) -> (
    if (instance(g, HMatrixGate) or instance(h, HMatrixGate)) then error "multiply HMatrixGates using productHMatrixGate";
    if g===zeroHGate or h===zeroHGate then zeroHGate else 
    if g===oneHGate then h else 
    if h===oneHGate then g else 
    new ProductHGate from {
        Inputs => (g,h)
        } 
    )
length ProductHGate := g -> 1
specialize (ProductHGate, InputValueTable) := (g, L) -> valueList { (specialize(first g.Inputs, L))#0 * (specialize(last g.Inputs, L))#0 }
diff (InputHGate, ProductHGate) := (x,g) -> (first g.Inputs)*diff(x,last g.Inputs) + (last g.Inputs)*diff(x,first g.Inputs)

DetHGate = new Type of HGate
net DetHGate := g -> (
    G := g.Input;
    concatenateNets {"det", net G}
    )
detHGate = method()
detHGate(HMatrixGate) := G -> (
    A := G.Elements;
    r := G.Rows;
    c := G.Cols;
    if r != c then error "Error, expecting a square matrix";
    if r == 1 then A#0 else (
        new DetHGate from {
            Input => G        
            }
    ))
length DetHGate := g -> 1
specialize (DetHGate, InputValueTable) := (g, L) -> (
    G := g.Input;
    row := G.Cols; 
    evalG := specialize(G, L); -- evaluates to a list of length (length G)
    squareMatrixList := toList (0..(row-1)) / (i -> (
        toList (0..(row-1)) / (j -> evalG#(i*row + j))
    )); -- convert to a list of lists
    squareMatrix := matrix squareMatrixList; -- convert to a matrix
    valueList {det squareMatrix}
    )

diff (InputHGate, DetHGate) := (x,g) -> (
    G := g.Input;
    A := G.Elements;
    n := G.Rows; 
    returnL := (0..n-1) / (i -> 
        detHGate(hMatrixGate (toList (0..(n*n-1)) / (j -> 
                if j >= i*n and j < (i+1)*n then diff(x, A#j) else A#j), n, n)));
    fold(plus, returnL)
    )


ElementHGate = new Type of HGate
net ElementHGate := g -> (
    G := first g.Inputs;
    i := last g.Inputs;
    if i < 0 then error "index < 0";

    concatenateNets {G, "[", i, "]"}
    )
length ElementHGate := g -> 1
specialize (ElementHGate, InputValueTable) := (g, L) -> (
    G := first g.Inputs;
    i := last g.Inputs;
    evalG := specialize(G, L); -- computationally not great, but preserves blackbox structure
    valueList { evalG#i }
    )
diff (InputHGate, ElementHGate) := (x,g) -> (
    diffG := diff(x, first g.Inputs); -- computationally not great, but preserves blackbox structure
    elementHGate(diffG, last g.Inputs) 
    )
elementHGate = method()
-- assumes single index to index an element
elementHGate (HMatrixGate, ZZ) := (G, i) -> (
    if i < 0 then error "index < 0";
    new ElementHGate from {
        Inputs => (G, i)
        }
    )

-- the following are HMatrixGates ------------------------------------------
-- an HMatrix Gate is a special type of HGate satisfying the form: \F^m -> \F^n
-- where n \geq 1

net HMatrixGate := G -> (
    A := G.Elements; -- List of HMatrixGates
    concatenateNets{"|", A, " (" , toString G.Rows , ", " , toString G.Cols , ")|"}
    )
length HMatrixGate := G -> G.Rows*G.Cols -- number of HMatrixGates in the matrix

-- flatten HMatrixGate into list of length 1 HGates
flatten HMatrixGate := G -> (
    A := G.Elements; 
    n := length G; -- total number of entries in the matrix
    if #A == n then return G; -- already flattened
    AFlatten := flatten {A/(e -> flatten e)}; -- flatten the list of HGates
    hMatrixGate(AFlatten, 1, n) -- return a new HMatrixGate with the flattened list
    -- row vector (1xn)
)
specialize (HMatrixGate, InputValueTable) := (G, L) -> (
    A := G.Elements;
    nestedEvalA := valueList A/(e -> specialize(e, L));
    evalA := flatten nestedEvalA; 
    evalA 
    )
hMatrixGate = method()  
hMatrixGate (List, ZZ, ZZ) := (A, r, c) -> (
    if not all(A, (e -> instance(e, HGate))) then error "input is not a list of HGates";
    --tempA := flatten {A/(e -> if length e == 1 then e 
    --                           else (tempe := flatten e;
    --                                tempe.Elements
    --                            ))};
    --AFlatten := flatten tempA; -- flatten the list of HMatrixGates
    new HMatrixGate from {
        Elements => A,
        Rows => r,
        Cols => c
        }
    )
diff (InputHGate, HMatrixGate) := (x,G) -> (
    A := G.Elements; -- List of HMatrixGates
    diffA := A/(e -> diff(x, e));
    hMatrixGate(diffA, G.Rows, G.Cols) -- diff each HMatrixGate in the list
    )
-- X must be an mx1 vector of InputHGates
-- G must be an nx1 vector of HGates, NOT HMatrixGates
jacobian (HMatrixGate, HMatrixGate) := (X, G) -> (
    if not instance(X, HMatrixGate) or not instance(G, HMatrixGate) then error "X, G must be HMatrixGates";
    if not all(X.Elements, (e -> instance(e, InputHGate))) then error "X is not a matrix of InputHGates";
    --if any(G.Elements, (g -> instance(g, HMatrixGate))) then error "G cannot contain HMatrixGates";
    if X.Cols != 1 or G.Cols != 1 then error "column must be 1";
    n := G.Rows;
    m := X.Rows;
    -- make column by column n x m matrix
    E := X.Elements / (x -> diff(x, G));

    flatListForMatrix := flatten (toList (0..(m-1)) / (i -> (
        toList(0..(n-1)) / (j -> elementHGate(E#j, i) 
        ))
    ));
    hMatrixGate(flatListForMatrix, n, m)
)

SumHMatrixGate = new Type of HMatrixGate
net SumHMatrixGate := S -> (
    G := first S.Inputs; 
    H := last S.Inputs; 

    "(" | net M | "+" | net N | ")"
    )
sumHMatrixGate = method()
sumHMatrixGate(HMatrixGate, HMatrixGate) := (G, H) -> (
    if G.Rows != H.Rows or G.Cols != H.Cols then error "G and H must have same dimensions";
    new SumHMatrixGate from {
            Inputs => (G, H),
            Rows => G.Rows,
            Cols => G.Cols
            }
    )
length SumHMatrixGate := S -> S.Rows * S.Cols
specialize (SumHMatrixGate, InputValueTable) := (S, L) -> (
    G := first S.Inputs; 
    H := last S.Inputs; 
    
    evalG := flatten specialize (G, L);
    evalH := flatten specialize (H, L);

    n := #evalG;
    valueList toList (0..(n-1))/(i -> evalG#i + evalH#i) 
    )
diff (InputHGate, SumHMatrixGate) := (x,S) -> (
    G := first S.Inputs; 
    H := last S.Inputs;

    sumHMatrixGate(diff(x, G), diff(x, H))
    )

ScalarProductHMatrixGate = new Type of HMatrixGate
net ScalarProductHMatrixGate := S -> (
    g := first S.Inputs; 
    H := last S.Inputs; 
    "(" | net g | "*" | net H | ")"
    )
scalarProductHMatrixGate = method()
scalarProductHMatrixGate(HGate, HMatrixGate) := (g, H) -> (
    if instance(g, HMatrixGate) then error "use productHMatrixGate for HMatrixGate * HMatrixGate";
    new ScalarProductHMatrixGate from {
            Inputs => (g, H),
            Rows => H.Rows,
            Cols => H.Cols
            }
    )
length ScalarProductHMatrixGate := S -> S.Rows * S.Cols
diff (InputHGate, ScalarProductHMatrixGate) := (x,S) -> (
    g := first S.Inputs; 
    H := last S.Inputs; 

    sumHMatrixGate(
        scalarProductHMatrixGate(g, diff(x, H)),
        scalarProductHMatrixGate(diff(x, g), H)
        ))
specialize (ScalarProductHMatrixGate, InputValueTable) := (S, L) -> (
    g := first S.Inputs; 
    H := last S.Inputs; 
    evalg := flatten specialize (g, L);
    evalH := flatten specialize (H, L);
    n := #evalH;
    valueList toList (0..(n-1))/(i -> evalH#i * evalg) 
    )


ProductHMatrixGate = new Type of HMatrixGate
net ProductHMatrixGate := S -> (
    G := first S.Inputs; 
    H := last S.Inputs; 
    "(" | net G | "*" | net G | ")"
    )
productHMatrixGate = method()
productHMatrixGate(HMatrixGate, HMatrixGate) := (G, H) -> (
    if G.Cols != H.Rows then error "G.Cols must equal H.Rows";
    new ProductHMatrixGate from {
            Inputs => (G, H),
            Rows => G.Rows,
            Cols => H.Cols
            }
    )
length ProductHMatrixGate := S -> (
    S.Rows * S.Cols
    )
specialize (ProductHMatrixGate, InputValueTable) := (S, L) -> (
    G := first S.Inputs;
    H := last S.Inputs; 
    n := G.Rows;
    k := G.Cols;
    m := H.Cols;
    evalG := specialize (G, L);
    evalH := specialize (H, L);
    listMatrixA := toList (0..(n-1)) / (i -> (
        toList (0..(k-1)) / (j -> evalG#(i*k + j))
    )); -- convert to a list of lists
    matrixA := matrix listMatrixA; 
    listMatrixB := toList (0..(k-1)) / (i -> (
        toList (0..(m-1)) / (j -> evalH#(i*m + j))
    )); -- convert to a list of lists
    matrixB := matrix listMatrixB;
    resultAxB := valueList flatten entries (matrixA * matrixB);
    resultAxB
    )
diff (InputHGate, ProductHMatrixGate) := (x,S) -> (
    G := first S.Inputs; 
    H := last S.Inputs; 

    sumHMatrixGate(
        productHMatrixGate(G, diff(x, H)),
        productHMatrixGate(diff(x, G), H)
        ))

SolveHMatrixGate = new Type of HMatrixGate
-- solves for x = A^{-1} b
-- assumes detA != 0
net SolveHMatrixGate := S -> (
    G := first S.Inputs; 
    H := last S.Inputs; 

    -- see overleaf for explanation
    concatenateNets {"solve(", G, ", ", H, ")"}
    
    )
solveHMatrixGate = method()
solveHMatrixGate(HMatrixGate, HMatrixGate) := (G, H) -> (
    n := length H; 
    if H.Cols != 1 then error "b must have 1 column";
    if G.Rows != n or G.Cols != n then error "A must be square, with dimension b.rows x b.rows";
    new SolveHMatrixGate from {
        Inputs => (G, H), 
        Rows => n,
        Cols => 1
        }
    )
length SolveHMatrixGate := S -> (
    S.Rows
)
specialize (SolveHMatrixGate, InputValueTable) := (S, L) -> (
    G := first S.Inputs;
    H := last S.Inputs;
    n := length S;
    evalG := specialize (G, L);
    evalH := specialize (H, L);
    listMatrixA := toList (0..(n-1)) / (i -> (
        toList (0..(n-1)) / (j -> evalG#(i*n + j))
    )); -- convert to a list of lists
    matrixA := matrix listMatrixA; 
    inverseMatrixA := inverse matrixA;
    listMatrixB := toList (0..(n-1)) / (i ->
        {evalH#i}
    ); -- convert to a list of lists
    matrixB := matrix listMatrixB;
    valueList flatten entries (inverseMatrixA * matrixB)
    )
diff (InputHGate, SolveHMatrixGate) := (x,S) -> (
    G := first S.Inputs; 
    H := last S.Inputs; 
    n := length S; 
    A := G.Elements;
    b := H.Elements;

    -- base case, 1x1 matrix, needs a divide gate
    doubleListA := toList(0..(n-1)) / (i -> (
        toList(0..(n-1)) / (j -> A#(i*n + j))
    ));
    -- SolveHMatrixGate for A and each partial diff column of A
    -- list of length n with SolveHMatrixGate entries
    partialOfA := toList (0..(n-1))/(i -> (
                solveHMatrixGate(G, hMatrixGate(toList(0..(n-1)) / (j -> diff(x, A#(i*n+j))), n, 1))));
    flatListForMatrix := flatten (toList (0..(n-1)) / (i -> (
        toList(0..(n-1)) / (j -> elementHGate(partialOfA#j, i) 
        ))
    ));
    colMatrixHMatrixGates := hMatrixGate(flatListForMatrix, n, n);

    -- convert list to vector
    partialOfb := toList b / (e -> diff(x, e));
    partialH := hMatrixGate(partialOfb, n, 1);

    -- see overleaf for explanation
    sumHMatrixGate(productHMatrixGate(colMatrixHMatrixGates, solveHMatrixGate(G, H)), solveHMatrixGate(G, partialH))
    
    )

-- method for getting x'(t) (see Section on Predictors in overleaf)
-- t: InputHGate for time
-- X: HMatrixGate column vector where Elements is a list of InputHGates
c = method()
c (InputHGate, HMatrixGate, HMatrixGate) := (t, X, F) -> (
    assert (X.Cols == 1 and all(X.Elements, (x -> instance(x, InputHGate))));

    dFdx := jacobian (X, F);
    dFdt := diff(t, F);
    solveHMatrixGate(dFdx, dFdt)
    )

predictorHMatrixGate = method()
-- based on the trapezoid predictor 
-- H denotes a holder variable to store values
-- note: H.Elements = {T0, X0}
-- T0.Elements = {t0, t1}
-- X0.Elements = {InputHGate, ..}
predictorHMatrixGate (HMatrixGate,
        InputHGate, HMatrixGate, HMatrixGate) := (H, t, X, F) -> (
    X0 := last H.Elements;
    n := length X0;
    T0 := first H.Elements;

    t0 := first T0.Elements;
    t1 := last T0.Elements;
    c1 := c(t, X, F);

    -- substitute t, X with t_0, X_0
    c2 := subGate (t, t0, c1);
    (0..n-1) / (i -> c2 = subGate((X.Elements)#i, (X0.Elements)#i, c2));

    tDelta := t1 - t0;
    cfirst := c2;
    h1 := scalarProductHMatrixGate(tDelta, cfirst);
    -- Xtang named after X value from tangent predictor
    Xtang := sumHMatrixGate(X0, h1); -- X_0 + c(X_0, t_0)*tDelta

    -- substitute t, X with t_0 + tDelta = t_1, Xtang
    c4 := subGate (t, t1, c1);
    (0..n-1) / (i -> c4 = subGate((X.Elements)#i, elementHGate(Xtang, i), c4));
    csecond := c4;

    h2 := (inputHGate 0.5) * t1; -- t1/2
    c5 := sumHMatrixGate(cfirst, csecond); -- c(X_0, t_0) + c(...)
    h3 := scalarProductHMatrixGate(h2, c5); -- [c(X_0, t_0) + c(...)]*(t1/2)
    sumHMatrixGate(X0, h3)
    )

HMap = new Type of HashTable
net HMap := H -> (
    concatenateNets {"HMap(", H.InputGates, ") =", H.OutputGates}
    )
hMap = method()
hMap(List, List) := (I, O) -> (
    -- assume that I contains variables in elements of O
    if not all(I, (e -> instance(e, HGate))) then error "input is not a list of HGates";
    if not all(O, (e -> instance(e, HGate))) then error "output is not a list of HGates";
    new HMap from {
            InputGates => I,
            OutputGates => O
            }
    )
-- Newton's method
newtonsOp = method()
newtonsOp(HMap) := g -> (
    -- assume g.Inputs#0 = X: HMatrixGate of variables, g.Outputs#0 = G: HMatrixGate of functions
    P := g.OutputGates#0;
    Y := g.InputGates#0;
    if not instance(P, HMatrixGate) or not instance(Y, HMatrixGate) then error "accepts only matrices";
    -- assume square situation
    if P.Rows != Y.Rows or P.Cols != Y.Cols or P.Cols != 1 then error "wrong dimensions";
    J := jacobian(Y, P);
    Y - solveHMatrixGate(J, P)
)

-- H version of Straight-line Programs ---------------------------------------------
-- substitute all instances of x with y 
-- G[x => y]
subGate = method()
-- either y is an InputHGate or an ElementHGate
subGate (InputHGate, HGate, HGate) := (x, y, G) -> (
    if instance(G, InputHGate) then (
        if toString x.Name == toString G.Name then y else G
    )
    else if instance(G, SumHGate) then (
        H1 := subGate(x, y, first G.Inputs);
        H2 := subGate(x, y, last G.Inputs);
        H1 + H2
    ) else if instance(G, ProductHGate) then (
        H3 := subGate(x, y, first G.Inputs);
        H4 := subGate(x, y, last G.Inputs);
        H3 * H4
    ) else if instance(G, DetHGate) then (
        H5 := subGate(x, y, G.Input);
    ) else if instance(G, ElementHGate) then (
        H6 := subGate(x, y, first G.Inputs);
        elementHGate(H6, last G.Inputs)
    ) else if instance(G, SumHMatrixGate) then (
        H7 := subGate(x, y, first G.Inputs);
        H8 := subGate(x, y, last G.Inputs);
        sumHMatrixGate(H7, H8)
    ) else if instance(G, ProductHMatrixGate) then (
        H9 := subGate(x, y, first G.Inputs);
        H10 := subGate(x, y, last G.Inputs);
        productHMatrixGate(H9, H10)
    ) else if instance(G, SolveHMatrixGate) then (
        H11 := subGate(x, y, first G.Inputs);
        H12 := subGate(x, y, last G.Inputs);
        solveHMatrixGate(H11, H12)
    ) else if instance(G, ScalarProductHMatrixGate) then (
        H13 := subGate(x, y, first G.Inputs);
        H14 := subGate(x, y, last G.Inputs);
        scalarProductHMatrixGate(H13, H14)
    ) else if instance(G, HMatrixGate) then ( -- note, HMatrixGate must be at the bottom
        A1 := G.Elements / (H -> subGate(x, y, H));
        hMatrixGate(A1, G.Rows, G.Cols)
    ) else error "sub not defined for this type of HGate"
    )

-- H[x => y]
subMap = method()
subMap (InputHGate, InputHGate, HMap) := (x, y, H) -> (
    hMap (H.InputGates / (g -> subGate(x, y, g)), H.OutputGates / (g -> subGate(x, y, g)))
)

-- printing functions for SLP
PrintIndices = new Type of MutableHashTable
newPrintIndices = assignmentSymbol -> (p := new PrintIndices; p#"assignmentSymbol"=assignmentSymbol; p#"#consts"=p#"#vars"=p#"#lines"=0; p#"gates" = new MutableHashTable; p)

-- note: use the class(X) function to return class of X
-- note: can also use showStructure class(X)
hGateType = method() -- return string of specific type of HMatrixGate 
hGateType (HGate) := g -> (
    if instance(g, InputHGate) then "InputHGate"
    else if instance(g, SumHGate) then "SumHGate"
    else if instance(g, ProductHGate) then "ProductHGate"
    else if instance(g, DetHGate) then "DetHGate"
    else if instance(g, SolveHMatrixGate) then "SolveHMatrixGate"
    else if instance(g, ElementHGate) then "ElementHGate"
    else if instance(g, SumHMatrixGate) then "SumHMatrixGate"
    else if instance(g, ProductHMatrixGate) then "ProductHMatrixGate"
    else if instance(g, ScalarProductHMatrixGate) then "ScalarProductHMatrixGate"
    else if instance(g, HMatrixGate) then "HMatrixGate"

    else "HGate" -- default case
    )

printSLP = method()
printSLP (List, List) := (I, O) -> (
    if not all(I, (e -> instance(e, HGate))) then error "Error, I is not a list of HGates";
    if not all(O, (e -> instance(e, HGate))) then error "Error, O is not a list of HGates";
    p := newPrintIndices " = ";
    O / (g -> printHGate(g, p));
    unsortedLines := values p#"gates" / (l -> l#0 | p#"assignmentSymbol" | l#1 );
    sort unsortedLines / (slpLine -> << slpLine << endl);
    O / (g -> << "OUTPUT: " << ((p#"gates")#g)#0 << p#"assignmentSymbol" << ((p#"gates")#g)#1  << endl);
    )

printHGate = method()
printHGate (InputHGate, PrintIndices) := (g,p) -> if (p#"gates")#?g then (p#"gates")#g else (
    if isConstant g then (
	    (p#"gates")#g = {"C"|toString p#"#consts", net g};
    	p#"#consts" = p#"#consts" + 1;
	) else (
	    (p#"gates")#g = {"I"|toString p#"#vars", net g};
    	p#"#vars" = p#"#vars" + 1;
	);
    (p#"gates")#g
    )



printHGate (SumHGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    a := g.Inputs#0;
    b := g.Inputs#1;
    val := (printHGate(a,p))#0 | "+" | (printHGate(b,p))#0;
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHGate (ProductHGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    a := g.Inputs#0;
    b := g.Inputs#1;
    val := (printHGate(a,p))#0 | "*" | (printHGate(b,p))#0;
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHGate (DetHGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    m := g.Inputs;
    val := "det(" | (printHGate(m,p))#0 | ")";
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHGate (SolveHMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    A := g.Inputs#0;
    b := g.Inputs#1;
    val := "solve{" | (printHGate(A,p))#0 | ", " | (printHGate(b,p))#0 | "}";
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))



printHGate (SumHMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    M := g.Inputs#0;
    N := g.Inputs#1;
    val := "matrixSum(" | (printHGate(M,p))#0 | ", " | (printHGate(N,p))#0 | ")";
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHGate (ProductHMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    M := g.Inputs#0;
    N := g.Inputs#1;

    val := "matrixProduct(" | (printHGate(M,p))#0 | ", " | (printHGate(N,p))#0 | ")";
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))


printHGate (ScalarProductHMatrixGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    M := g.Inputs#0;
    N := g.Inputs#1;

    val := "scalarMatrixProduct(" | (printHGate(M,p))#0 | ", " | (printHGate(N,p))#0 | ")";
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHGate (ElementHGate, PrintIndices) := (g,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (
    M := g.Inputs#0;
    i := g.Inputs#1;

    c := inputHMatrixGate i;

    if not (p#"gates")#?c then (
        (p#"gates")#c = {"C"|toString p#"#consts", net c};
        p#"#consts" = p#"#consts" + 1;  

    );

    val := (printHGate(M,p))#0 | "[" | ((p#"gates")#c)#0 | "]";
    idx := "R"|toString p#"#lines";
    (p#"gates")#g = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#g
    ))

printHGate (HMatrixGate, PrintIndices) := (G,p) -> (
    if (p#"gates")#?g then (p#"gates")#g else (

    A := G.Elements;

    listLines := A / (h -> (printHGate(h,p))#0);

    val := "matrix" | toString listLines | " (" | toString G.Rows | ", " | toString G.Cols | ")";

    idx := "R"|toString p#"#lines";
    (p#"gates")#G = {idx, val};
    p#"#lines" = p#"#lines" + 1;
    (p#"gates")#G
    ))

end
