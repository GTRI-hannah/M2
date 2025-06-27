Types:

Currently
* `HGate` is the base Type
* `InputHGate`, `SumHGate`, `ProductHGate`, `DetHGate`
* `HMatrix` is an `HGate` with additional properties
    - knows number of rows and columns
* `SumHMatrix`, `ProductHMatrix` inherit from `HMatrix`

Proposal:
* `HMatrixGate` is a base Type
  - define `flatten`  
  - constructor should be able to take any data (a list of `HMatrixGates`) as inputs: e.g `{InputHGate, SumHGate, SubmatrixGate, DetGate, HMatrixGate, SolveHMatrix}` 
* `InputHGate`, `SumHGate`, `ProductHGate`, `DetHGate`, `SubmatrixHGate` inherit from `HMatrixGate`
* `diff` method that takes `HMatrixGate` 

Suppose $f(x)$, a usual function, is a program with $m$ arithmetic gates.
What is the upper bound for the number of gates in $df/dx(x)$ produced by auto-differentiation?