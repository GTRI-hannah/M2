-- check predicate
restart
needs "../HGates.m2"
declareVariable \ {x, y, z}

g_1 = x * y 
g_2 = g_1 * g_1
h_1 = hMap({x}, {g_2})
h_2 = hMap({x, y}, {g_2})
h_3 = hMap({x}, {hMatrixGate({g_1, g_2}, 2, 1)})

subMap (x, z, h_2)
