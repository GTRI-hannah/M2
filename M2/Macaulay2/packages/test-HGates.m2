restart
load "HGates.m2"
declareVariable x
declareVariable y
declareVariable z
declareVariable w
detHGate(2, {x, y, z, oneHGate})
detHGate(2, {x, y, z, zeroHGate})
detHGate(2, {x, y, z, w})

A = {x, x, x, zeroHGate, y, y, zeroHGate, zeroHGate, z}
-- fails: seems to convert some gates to strings
detHGate(3, A)

-- fails: same issue, reads gate as string
detHGate(2, {x, y, z, oneHGate}) * detHGate(2, {x, y, oneHGate, zeroHGate})

-- fails: same issue, reads gate as string
solveHGate(2, {x, y, z, oneHGate}, {w, w})
