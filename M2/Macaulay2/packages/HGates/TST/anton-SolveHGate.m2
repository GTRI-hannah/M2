S = solveHGate(1,{x},{oneHGate}) -- error
S = solveHGate(2,{x,zeroHGate,zeroHGate,oneHGate},{oneHGate,oneHGate}) 
Sx = diff(x,S) -- incorrect ... what is "matrix{" ?
class Sx -- why Net?

S = solveHGate(2,{x,y,z,w},{oneHGate,oneHGate})
Sx = diff(x,S)