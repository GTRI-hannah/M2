restart
needs "../HGates.m2"
declareVariable \ {x,y,z,w}
x0 = inputValueTable {x => 1, y => pi, z => 0, w => 1/2}
specialize(x,x0)
specialize(x+y,x0)
R = QQ[X,Y,Z,W]
x0 = inputValueTable {x => X, y => Y, z => Z, w => W}
specialize(x,x0)
specialize(x+y,x0)
specialize(x*y,x0)
specialize(x*y*z+w,x0)


