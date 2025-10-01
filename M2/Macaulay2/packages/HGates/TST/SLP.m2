restart
needs "../HGates.m2"
declareVariable \ {x, y}
a = inputHGate 4
b = x*x*a
c = b*b
d = c + y
printSLP({x, y}, {d})

restart
needs "../HGates.m2"
declareVariable \ {x, y}
a = inputHGate 3
b = hMatrixGate({x, a, y, zeroHGate}, 2, 2)
c = detHGate(b)
d = c + a
e = hMatrixGate({x*x, x*y+y*y}, 2, 1)
f = solveHMatrixGate(b, e)
g = elementHGate(f, 0)
h = elementHGate(f, 1)
i = g+h
printSLP ({x, y}, {d, i})

restart
needs "../HGates.m2"
declareVariable \ {x, y, t}
X = hMatrixGate({x, y}, 2, 1)
F = hMatrixGate({x*x*t+y*t, y*y*y*t+oneHGate*t}, 2, 1)
H = hMap({t, X}, {F})
X0 = hMatrixGate({inputHGate 1., inputHGate 2.}, 2, 1)
t0 = inputHGate 0.1
t1 = inputHGate 0.15
P = predictorRK4HMatrixGate(H, {t0, t1, X0});
O = (newtonsOp(subMap(t, t1, H))).OutputGates#0;
printSLP ({x, y, t}, {P, O})


restart
needs "../HGates.m2"
declareVariable \ {x, y}
a = inputHGate 4
b = x*x*a
c = b*b
d = c + y
I = {x, y}
O = {d}
--printSLP(I, O)

isExternalCCppLibraryUsable = method()
-- input dummy Boolean variable
isExternalCCppLibraryUsable Boolean := v -> (
    w := (run "gcc --version" == 0);
    if w then (
	print "-- Found `gcc`.";
	) else (
	print "-- Couldn't find `gcc`, no capability to execut with external C/C++ libraries";
	);
        w
    ) 
isExternalCCppLibraryUsable true

printSLPToFile = method()
printSLPToFile (List, List, File) := (I, O, f) -> (
    f << "void evaluate(const C* x, C* y) {" << endl;
    if not all(I, (e -> instance(e, HGate))) then error "Error, I is not a list of HGates";
    if not all(O, (e -> instance(e, HGate))) then error "Error, O is not a list of HGates";
    p := newPrintIndices " = ";
    O / (g -> printHGate(g, p));
    unsortedLines := values p#"gates" / (l -> 
        if "C" == (l#0)#0 or "I" == (l#0)#0 or true then ("C " |l#0 | p#"assignmentSymbol" | l#1) --remove true
        else (l#0 | p#"assignmentSymbol" | l#1) );
    
    sort unsortedLines / (slpLine -> f << slpLine << ";" << endl);
    (0..#O-1) / (i -> << "y[" << i << "]" << p#"assignmentSymbol" << ((p#"gates")#(O#i))#0 << ";" << endl);
    (0..#O-1) / (i -> f << "y[" << i << "]" << p#"assignmentSymbol" << ((p#"gates")#(O#i))#0 << ";" << endl);
    f << "}" << endl;
    )

K = RR_53
typeName := (
if K === RR_53 then "double" else 
if K === CC_53 then "std::complex<double>" else 
    error ("HSLP is not implemented for "| toString K) 
    )
fname := temporaryFileName() | "-GateSystem";
cppName := fname | ".cpp"
--cppName := fname | ".c";
libName := fname | ".so"
f := openOut cppName
f << "#include <complex>" << endl; 
f << "std::complex<double> ii(0,1);" << endl;
f << "typedef " | typeName | " C;" << endl; -- << "extern" << endl; -- the type needs to be adjusted!!!
printSLPToFile(I, O, f)
f << close;
compileCommand := "g++ -shared -Wall -fPIC -Wextra -O3 -o "| libName | " " | cppName
print compileCommand
if run compileCommand > 0 then error ("error compiling a straightline program:\n"|compileCommand)
print get cppName
print libName
symNames := get ("!nm "|libName)
(a,b) := first regex("[0-9a-zA-Z_]*evaluate[0-9a-zA-Z_]*", symNames)
print ("mangled function name: "|substring(symNames,a,b))

-- how do I do this
--slp.cache#K = rawCompiledSLEvaluator(libName, #(slp#"input"), #(slp#"output"),
--    raw mutableMatrix(K,0,0) -- we need to pass only the field 
--    )
