R = QQ[x, y, A, B, C, D, E, s5]
f1 = 81*A + 9*D + 1
f2 = A - D + 1
f3 = (4*x-8)^2*A - (4*x-8)*(9/5)*B + (9/5)^2*C + (4*x-8)*D - (9/5)*E + 1
f4 = (4 - s5*5/3)^2*A + (4-s5*5/3)*(5*y/2 - 1/2)*B + (5*y/2 - 1/2)^2*C + (4-s5*5/3)*D + (5*y/2-1/2)*E + 1
f5 = 4*x^2*A - 6*x*y*B + 9*y^2*C + 2*x*D - 3*y*E + 1
f6 = A - C - D^2/4 + E^2/4
f7 = B/2 - D*E/4
f8 = s5^2 - 5
I = ideal(f1, f2, f3, f4, f5, f6, f7, f8)
dim I 
degree I

loadPackage "NumericalAlgebraicGeometry"

M = matrix{{1, 5.}, {0, 5.}}
invM = inverse M
norm (2, M)
 
