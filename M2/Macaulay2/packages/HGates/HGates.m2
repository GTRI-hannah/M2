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
diff (InputHGate, ProductHGate) := (x,g) -> (first g.Inputs)*diff(x,last g.Inputs) + (last g.Inputs)*diff(x,first g.Inputs)

-- from SPLexpressions, for printing
concatenateNets = method()
concatenateNets List := L -> (
    result := net "";
    for a in L do result = result | net a;
    result
    )

-- Define a new type for abstract HGates matrices
HMatrix = new Type of HashTable
net HMatrix := g -> (
    A := g.Elements; -- data array
    r := g.Rows; -- number of rows
    c := g.Columns; -- number of columns

    -- print matrix (r x c)
    doubleListA := toList(0..(r-1)) / (i -> (
        toList(0..(c-1)) / (j -> A#(i*c + j))
    ));
    concatenateNets {"matrix", MatrixExpression applyTable(doubleListA, net)}
    )
length HMatrix := g -> g.Rows * g.Columns -- number of HGates in the matrix
hMatrix = method()  
hMatrix (List, ZZ, ZZ) := (A, r, c) -> (
    if #A != r*c then error "data array is not matching the size of the matrix";
    if not all(A, (e -> instance(e, HGate))) then error "data array is not a list of HGates";
    new HMatrix from {
        Elements => A,
        Rows => r,
        Columns => c
        }
    )

hVector = method()
hVector (List) := (L) -> (
    if not all(L, (e -> instance(e, HGate))) then error "data array is not a list of HGates";
    new HMatrix from {
        Elements => L,
        Rows => #L,
        Columns => 1
        }
    )

getHGate = method()
getHGate (HMatrix, ZZ, ZZ) := (M, i, j) -> (
    if i < 0 or i >= M.Rows then error "row index out of bounds";
    if j < 0 or j >= M.Columns then error "column index out of bounds";
    M.Elements#(i * M.Columns + j)
    )

diff (InputHGate, HMatrix) := (x,g) -> (
    r := g.Rows;
    c := g.Columns;
    A := g.Elements;

    -- assumes A is flattened
    -- diffs each HGate
    partialOfA := A/(e -> diff(x, e));
    -- see overleaf for explanation
    hMatrix(partialOfA, r, c)
    )

SumHMatrix = new Type of HMatrix
net SumHMatrix := g -> "(" | net first g.Inputs | "+" | net last g.Inputs | ")"
HMatrix + HMatrix := (M, N) -> (
    -- removing this bc some types (eg SolveHGate) don't have Rows and Columns information
    --if M.Rows != N.Rows or M.Columns != N.Columns then error "matrices are not the same size";
    new SumHMatrix from {
      	Inputs => (M, N)
      	} 
    )
length SumHMatrix := g -> length first g.Inputs
diff (InputHGate, SumHMatrix) := (x,g) -> diff(x,first g.Inputs) + diff(x,last g.Inputs)

ProductHMatrix = new Type of HMatrix
net ProductHMatrix := g -> "(" | net first g.Inputs | "*" | net last g.Inputs | ")"
HMatrix * HMatrix := (M, N) -> (
    -- removing this bc some types (eg SolveHGate) don't have Rows and Columns information
    --if M.Columns != N.Rows then error "matrices are not compatible for multiplication";
    new ProductHMatrix from {
        Inputs => (M, N)
        } 
    )
length ProductHMatrix := g -> first g.Inputs.Rows * last g.Inputs.Columns -- number of HGates in the resulting matrix
diff (InputHGate, ProductHMatrix) := (x,g) -> (first g.Inputs)*diff(x,last g.Inputs) + (last g.Inputs)*diff(x,first g.Inputs)


DetHGate = new Type of HGate
net DetHGate := g -> (
    M := g.Inputs;
    n := M.Rows; -- assume M is a square matrix
    A := M.Elements;

    -- print square matrix (n x n)
    doubleListA := toList(0..(n-1)) / (i -> (
        toList(0..(n-1)) / (j -> A#(i*n + j))
    ));
    concatenateNets {"det", MatrixExpression applyTable(doubleListA, net)}

    )
detHGate = method()
detHGate(HMatrix) := M -> (
    A := M.Elements;
    n := M.Rows; -- assume M is a square matrix
    if #A != n^2 then error "data array is not matching the size of the matrix";
    if n == 1 then A#0 else (
    new DetHGate from {
        Inputs => M        
        }
    ))
length DetHGate := g -> 1
diff (InputHGate, DetHGate) := (x,g) -> (
    M := g.Inputs;
    n := M.Rows; -- assume M is a square matrix
    A := M.Elements;
    returnL := (0..n-1) / (i -> 
        detHGate(hMatrix (toList (0..(n*n-1)) / (j -> 
                if j >= i*n and j < (i+1)*n then diff(x, A#j) else A#j), n, n)));
    fold(plus, returnL)
    )

HMatrixElementHGate = new Type of HGate
net HMatrixElementHGate := g -> (
    concatenateNets {"(", net g.Source, ")[", g.Index, "]"}
    )
hMatrixElementHGate = method()
hMatrixElementHGate (HMatrix, ZZ) := (M, i) -> (
    new HMatrixElementHGate from {
        Source => M,
        Index => i
        }
    )


SolveHGate = new Type of HMatrix
-- solves for x = A^{-1} b
-- assumes detA != 0
net SolveHGate := g -> (
    M := first g.Inputs; -- HMatrix
    N := last g.Inputs; -- HMatrix
    n := M.Rows; -- assume length b is n (from solveHGate assertion)
    A := M.Elements;
    b := N.Elements;

    -- print square matrix (n x n)
    doubleListA := toList(0..(n-1)) / (i -> (
        toList(0..(n-1)) / (j -> A#(i*n + j))
    ));
    doubleListb := toList(0..(n-1)) / (i -> {b#i});

    -- see overleaf for explanation
    concatenateNets {"solve(", MatrixExpression applyTable(doubleListA, net), ", ", MatrixExpression applyTable(doubleListb, net), ")"}
    
    )
solveHGate = method()
solveHGate(HMatrix, HMatrix) := (M, N) -> (
    A := M.Elements;
    b := N.Elements;
    n := M.Rows; -- assume M is a square matrix

    if #A != n^2 then error "`A` data array is not matching the expected size of the matrix";
    if #b != n then error "`b` data array is not matching the expected size of the matrix";
    if N.Columns != 1 then error "`b` must be a column vector";
    if M.Rows != n then error "`A` must be a square matrix";
    new SolveHGate from {
        Inputs => (M, N),
        Rows => M.Columns,
        Columns => 1      
        }
    )
length SolveHGate := g -> g.Rows

diff (InputHGate, SolveHGate) := (x,g) -> (
    M := first g.Inputs; -- HMatrix
    N := last g.Inputs; -- HMatrix
    n := M.Rows; -- assume length b is n (from solveHGate assertion)
    A := M.Elements;
    b := N.Elements;

    -- base case, 1x1 matrix, needs a divide gate
    doubleListA := toList(0..(n-1)) / (i -> (
        toList(0..(n-1)) / (j -> A#(i*n + j))
    ));
    -- SolveHGate for A and each partial diff column of A
    -- list of length n with SolveHGate entries
    partialOfA := toList (0..(n-1))/(i -> (
                solveHGate(M, hVector(toList(0..(n-1)) / (j -> diff(x, A#(i*n+j)))))));

    flatListForMatrix := flatten (toList (0..(n-1)) / (i -> (
        toList(0..(n-1)) / (j -> hMatrixElementHGate(partialOfA#j, i) 
        ))
    ));

    colMatrixHGates = hMatrix(flatListForMatrix, n, n);

    -- convert list to vector
    partialOfb := toList b / (e -> diff(x, e));
    partialN := hVector(partialOfb);

    -- see overleaf for explanation
    (colMatrixHGates * solveHGate(M, N)) + solveHGate(M, partialN)
    
    )

end
