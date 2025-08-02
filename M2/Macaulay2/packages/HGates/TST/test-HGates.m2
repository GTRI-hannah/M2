restart
needs "../HGates.m2"
declareVariable \ {x,y,z,w}
x  -- expect InputHMatrixGate
x + y -- expect SumHMatrixGate
h = hMatrixGate({x,y}, 2, 1)
j = hMatrixGate({h, z,w}, 2, 2)
k = hMatrixGate({h, z+x*x, x+x, j}, 4, 2) 
diff(x, h) 
diff(x, j)
diff(x, k)
j + k -- expect error
bigSumHMatrixGate(j, j) 

h = hMatrixGate({x,y}, 2, 1)
j = hMatrixGate({h, z,w}, 2, 2)
s = solveHMatrixGate(j, h) 
diff(x, s)