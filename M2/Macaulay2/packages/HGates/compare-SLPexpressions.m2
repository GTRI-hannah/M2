restart
needsPackage "SLPexpressions"
declareVariable \ {x,y,z,w}
D = det matrix{{x,y},{z,w}}
diff(x,D)
