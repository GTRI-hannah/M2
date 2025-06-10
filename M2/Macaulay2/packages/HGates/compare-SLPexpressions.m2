restart
needsPackage "SLPexpressions"
declareVariable \ {x,y,z,w}
D = det matrix{{x,y},{z,w}}
diff(x,D)

restart
needs "HGates.m2"
declareVariable \ {x,y,z,w}
D = detHGate(2, {x,y,z,w})
diff(x,D)

