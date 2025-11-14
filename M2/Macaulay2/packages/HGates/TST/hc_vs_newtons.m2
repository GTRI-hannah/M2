
restart
load "conics/circle_two_sols_NM.m2"
fname = "finaltests/c_NM.txt"
f = openOut fname
f << "Circle Four Solutions via Newton's Method" << endl;
f << "Target: {-.25, 0, -.25, 0, 0} with 4 points (+/-1.414, +/1.414)" << endl;

<< "Newton's Method Circle" << endl;
num_sols = 0;
time for j from 1 to 112 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;
    x0 = random CC;
    y0 = random CC;
    f << "Starting at: " << (x0, y0) << endl;
    L = elapsedTiming (newtonsMethod(F, {x0, y0}, iterations => 50));
    X1newtime = L#0;
    X1new = L#1;
    f << "time: " << X1newtime << ", value: " << X1new << endl;
    vT = specialize(F, inputValueTable {x => X1new#0, y => X1new#1});
    X = hMatrixGate({x, y},2,1);
    Ff = F.OutputGates#0;
    JF = specialize(jacobian(X, Ff), inputValueTable (toList (0..(#X.Elements - 1))/(i -> (X.Elements)#i => X1new#i)));
    condJF = twonorm (JF);
    if (sqrt(vT#0*conjugate(vT#0) + vT#1*conjugate(vT#1)) < 1.0e-6) then (
        << "Run: " << j << ", Solution: " << num_sols << endl;
        if (condJF < 1.0e3) then (
            f << "-- F is close to zero at X1hom with good condition number: " << X1new << ", cond(J_F): " << condJF << endl;
        ) else (
            f << "-- F is close to zero at X1hom but with poor condition number: " << X1new << ", cond(J_F): " << condJF << endl;
            << "-- F is close to zero at X1hom but with poor condition number: " << X1new << ", cond(J_F): " << condJF << endl;
        );
        num_sols = num_sols + 1;
    );
)
<< "Total solutions found: " << num_sols << endl;
f << close;


restart
load "conics/ellipse1_two_sols_NM.m2"
fname = "finaltests/e_NM.txt"
f = openOut fname
f << "Ellipse1 Two Solutions via Newton's Method" << endl;
f << "Target: {-.111, 0, -.309, .889, 0} with points (2,1) or (4,-.6)" << endl;
-- try Newton, expect (2,1) or (4,-3/5)
-- newtonsMethod(F, {2, 1})
-- newtonsMethod(F, {4, -3/5})
<< "Newton's Method Ellipse1" << endl;
num_sols = 0;
time for j from 1 to 112 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;
    x0 = random CC;
    y0 = random CC;
    f << "Starting at: " << (x0, y0) << endl;
    L = elapsedTiming (newtonsMethod(F, {x0, y0}, iterations => 50));
    X1newtime = L#0;
    X1new = L#1;
    f << "time: " << X1newtime << ", value: " << X1new << endl;
    vT = specialize(F, inputValueTable {x => X1new#0, y => X1new#1});
    X = hMatrixGate({x, y},2,1);
    Ff = F.OutputGates#0;
    JF = specialize(jacobian(X, Ff), inputValueTable (toList (0..(#X.Elements - 1))/(i -> (X.Elements)#i => X1new#i)));
    condJF = twonorm (JF);
    if (sqrt(vT#0*conjugate(vT#0) + vT#1*conjugate(vT#1)) < 1.0e-6) then (
        << "Run: " << j << ", Solution: " << num_sols << endl;
        if (condJF < 1.0e3) then (
            f << "-- F is close to zero at X1hom with good condition number: " << X1new << ", cond(J_F): " << condJF << endl;
            << "-- F is close to zero at X1hom with good condition number: " << X1new << ", cond(J_F): " << condJF << endl;
        ) else (
            f << "-- F is close to zero at X1hom but with poor condition number: " << X1new << ", cond(J_F): " << condJF << endl;
            << "-- F is close to zero at X1hom but with poor condition number: " << X1new << ", cond(J_F): " << condJF << endl;
        );
        num_sols = num_sols + 1;
    );
)
<< "Total solutions found: " << num_sols << ", total target solutions found: " << num_targs << ", both targets: " << (found1 and found2) << endl;
f << close;

restart
load "conics/parabola_two_sols_NM.m2"
fname = "finaltests/p_NM.txt"
f = openOut fname
f << "Parabola Two Solutions via Newton's Method" << endl;
f << "Target: {-.125, .25, -.125, -.7071, -.7071} with points (-2.82843, 1) or (1.41421, 0.5)" << endl;
-- try homotopy continutation
-- try Newton, expect (-2sqrt(2), 1) or (sqrt(2), 1/2)
-- ie: (-2.82843, 1) or (1.41421, 0.5)
-- newtonsMethod(F, {-2.82843, 1})
-- newtonsMethod(F, {1.41421, 0.5}) -- getting -34.033, .4275 :(
<< "Newton's Method Parabola" << endl;
num_sols = 0;
time for j from 1 to 30 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;
    x0 = random CC;
    y0 = random CC;
    f << "Starting at: " << (x0, y0) << endl;
    L = elapsedTiming (newtonsMethod(F, {x0, y0}, iterations => 50));
    X1newtime = L#0;
    X1new = L#1;
    f << "time: " << X1newtime << ", value: " << X1new << endl;
    vT = specialize(F, inputValueTable {x => X1new#0, y => X1new#1});
    X = hMatrixGate({x, y},2,1);
    Ff = F.OutputGates#0;
    JF = specialize(jacobian(X, Ff), inputValueTable (toList (0..(#X.Elements - 1))/(i -> (X.Elements)#i => X1new#i)));
    condJF = twonorm (JF);
    if (sqrt(vT#0*conjugate(vT#0) + vT#1*conjugate(vT#1)) < 1.0e-6) then (
        << "Run: " << j << ", Solution: " << num_sols << endl;
        if (condJF < 1.0e3) then (
            f << "-- F is close to zero at X1hom with good condition number: " << X1new << ", cond(J_F): " << condJF << endl;
            << "-- F is close to zero at X1hom with good condition number: " << X1new << ", cond(J_F): " << condJF << endl;
        ) else (
            f << "-- F is close to zero at X1hom but with poor condition number: " << X1new << ", cond(J_F): " << condJF << endl;
            << "-- F is close to zero at X1hom but with poor condition number: " << X1new << ", cond(J_F): " << condJF << endl;
        );
        num_sols = num_sols + 1;
    );
)
<< "Total solutions found: " << num_sols << ", total target solutions found: " << num_targs << ", both targets: " << (found1 and found2) << endl;
f << close;

restart
load "conics/hyperbola_two_sols_NM.m2"
fname = "finaltests/h_NM.txt"
f = openOut fname
f << "Hyperbola Two Solutions via Newton's Method" << endl;
f << "Target:  {.2, 0, -.16, -1.2, 0} with points (3, 1.1180) or (3.5, 1.7321) " << endl;
-- try Newton
-- newtonsMethod(F, {3, 1.1180}) -- close ish
-- newtonsMethod(F, {3.5, 1.7321}) -- not good
<< "Newton's Method Hyperbola" << endl;
num_sols = 0;
time for j from 1 to 30 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;
    x0 = random CC;
    y0 = random CC;
    f << "Starting at: " << (x0, y0) << endl;
    L = elapsedTiming (newtonsMethod(F, {x0, y0}, iterations => 50));
    X1newtime = L#0;
    X1new = L#1;
    f << "time: " << X1newtime << ", value: " << X1new << endl;
    vT = specialize(F, inputValueTable {x => X1new#0, y => X1new#1});
    X = hMatrixGate({x, y},2,1);
    Ff = F.OutputGates#0;
    JF = specialize(jacobian(X, Ff), inputValueTable (toList (0..(#X.Elements - 1))/(i -> (X.Elements)#i => X1new#i)));
    condJF = twonorm (JF);
    if (sqrt(vT#0*conjugate(vT#0) + vT#1*conjugate(vT#1)) < 1.0e-6) then (
        << "Run: " << j << ", Solution: " << num_sols << endl;
        if (condJF < 1.0e3) then (
            f << "-- F is close to zero at X1hom with good condition number: " << X1new << ", cond(J_F): " << condJF << endl;
            << "-- F is close to zero at X1hom with good condition number: " << X1new << ", cond(J_F): " << condJF << endl;
        ) else (
            f << "-- F is close to zero at X1hom but with poor condition number: " << X1new << ", cond(J_F): " << condJF << endl;
            << "-- F is close to zero at X1hom but with poor condition number: " << X1new << ", cond(J_F): " << condJF << endl;
        );
        num_sols = num_sols + 1;
    );
)
<< "Total solutions found: " << num_sols << endl;
f << close;