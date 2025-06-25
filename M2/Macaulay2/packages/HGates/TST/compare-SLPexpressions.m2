restart
needsPackage "SLPexpressions"
declareVariable \ {x,y,z,w}
diff(x,x+y)
D = det matrix{{x,y},{z,w}}
diff(x,D)
D = det matrix{{x+y,y+z,z+w},{y*x,z*y,w*x},{x+1,y+1,z+1}}
diff(x,D)

restart
needs "../HGates.m2"
declareVariable \ {x,y,z,w}
M = hMatrix({x,y,z,w}, 2, 2)
D = detHGate(M)
diff(x,D)
D = detHGate(3,{x+y,y+z,z+w,y*x,z*y,w*x,x+oneHGate,y+oneHGate,z+oneHGate})
diff(x,D)
