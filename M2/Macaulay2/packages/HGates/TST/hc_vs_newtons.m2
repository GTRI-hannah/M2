
restart
load "conics/circle_two_sols_NM.m2"
fname = "tests/circle_two_sols_NM.txt"
f = openOut fname
f << "Circle Four Solutions via Newton's Method" << endl;
f << "Target: {-.25, 0, -.25, 0, 0} with 4 points (+/-1.414, +/1.414)" << endl;
-- try Newton
-- expect +-sqrt(2), +-sqrt(2)
-- newtonsMethod(F, {-sqrt(2), -sqrt(2)})
-- newtonsMethod(F, {-sqrt(2), sqrt(2)})
-- newtonsMethod(F, {sqrt(2), -sqrt(2)})
-- newtonsMethod(F, {sqrt(2), sqrt(2)})
<< "Newton's Method Circle" << endl;
time for j from 1 to 100 do (
    << "Trial " << j << endl;
    f << "Trial " << j << endl;
    x0 = random (-sqrt(2)-0.1, sqrt(2)+0.1);
    y0 = random (-sqrt(2)-0.1, sqrt(2)+0.1);
    f << "Starting at: " << (x0, y0) << endl;
    L = elapsedTiming (newtonsMethod(F, {x0, y0}));
    X1newtime = L#0;
    X1new = L#1;
    f << "time: " << X1newtime << ", value: " << X1new << endl;
    << "time: " << X1newtime << ", value: " << X1new << endl;
    vT = specialize(F, inputValueTable {x => X1new#0, y => X1new#1});
    if (sqrt(vT#0*vT#0 + vT#1*vT#1) < 0.06) then (
        f << "!!!!!!!! Found close enough solution: " << X1new << endl;
    );
)
f << close;

restart
load "conics/circle_two_sols_HC.m2"
fname = "tests/circle_two_sols_HC.txt"
f = openOut fname
f << "Circle Four Solutions via Homotopy Continuation" << endl;
f << "Target: {-.25, 0, -.25, 0, 0} with 4 points (+/-1.414, +/1.414)" << endl;
-- try homotopy continutation
-- try homotopy continutation
<< "HC Circle" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {-0.25, 0., -0.25, 0., 0.};
    ABCDEapprox = toList (0..4)/(i -> X1hom#i); 
    twonormdiff2 = sqrt fold(plus, (ABCDEapprox - ABCDEtrue)/(i -> i*i));

    X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
    twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*i));
    if (twonormdiff1 < 0.06) then (
        f << "-- F is close to zero at X1hom: " << X1hom << endl;
    );
    if (twonormdiff2 < 0.06) then (
        f << "-- Close to true conic coeffis: " << X1hom << endl;
    );
    f << "----------------------------------------" << endl;
)
f << close;


restart
load "conics/circle_two_sols_HC2.m2"
time for j from 1 to 5 do (
    fname = concatenate("tests/circle_two_sols_HC2_", toString j, ".txt");
    f = openOut fname;
    f << "Circle Two Solutions via Homotopy Continuation (2x2)" << endl;
    f << "Target: {-.25, 0, -.25, 0, 0} with 4 points (+/-1.414, +/1.414)" << endl;
    -- try homotopy continutation
    << "HC2 Circle" << endl;
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {-0.25, 0., -0.25, 0., 0.};
    ABCDEapprox = specialize(Y, inputValueTable {x => X1hom#0, y => X1hom#1}); 
    twonormdiff2 = sqrt fold(plus, (ABCDEapprox - ABCDEtrue)/(i -> i*i));

    X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
    twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*i));
    f << "2-norm coeffs: " << twonormdiff2 << endl;
    
    if (twonormdiff1 < 0.06) then (
        f << "-- F is close to zero at X1hom: " << X1hom << endl;
    );

    if (twonormdiff2 < 0.06) then (
        f << "Close to true conic coeffis: " << X1hom << endl;
    );
    f << "----------------------------------------" << endl;
    f << close;
)

restart
load "conics/ellipse1_two_sols_NM.m2"
fname = "tests/ellipse1_two_sols_NM.txt"
f = openOut fname
f << "Ellipse1 Two Solutions via Newton's Method" << endl;
f << "Target: {-.111, 0, -.309, .889, 0} with points (2,1) or (4,-.6)" << endl;
-- try Newton, expect (2,1) or (4,-3/5)
-- newtonsMethod(F, {2, 1})
-- newtonsMethod(F, {4, -3/5})
<< "Newton's Method Ellipse1" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;
    x0 = random (1.9, 4.1);
    y0 = random (-0.7, 1.1);
    f << "Starting at: " << (x0, y0) << endl;
    L = elapsedTiming (newtonsMethod(F, {x0, y0}));
    X1newtime = L#0;
    X1new = L#1;
    f << "time: " << X1newtime << ", value: " << X1new << endl;
    << "time: " << X1newtime << ", value: " << X1new << endl;
    vT = specialize(F, inputValueTable {x => X1new#0, y => X1new#1});
    if (sqrt(vT#0*vT#0 + vT#1*vT#1) < 0.06) then (
        f << "!!!!!!!! Found close enough solution: " << X1new << endl;
    );
)
f << close;

restart
load "conics/ellipse1_two_sols_HC.m2"
fname = "tests/ellipse1_two_sols_HC.txt"
f = openOut fname
f << "Ellipse1 Two Solutions via Homotopy Continuation" << endl;
f << "Target: {-.111, 0, -.309, .889, 0} with points (2,1) or (4,-.6)" << endl;
-- try homotopy continutation
<< "HC Ellipse1" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {-1/9, 0., -25/81, 8/9, 0.};
    ABCDEapprox = toList (0..4)/(i -> X1hom#i); 
    twonormdiff2 = sqrt fold(plus, (ABCDEapprox - ABCDEtrue)/(i -> i*i));

    X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
    twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*i));
    if (twonormdiff1 < 0.06) then (
        f << "-- F is close to zero at X1hom: " << X1hom << endl;
    );
    if (twonormdiff2 < 0.06) then (
        f << "-- !!!!!!!! Actually close to true conic coefficients: " << X1hom << endl;
    );
    f << "----------------------------------------" << endl;
)
f << close;


restart
load "conics/ellipse1_two_sols_HC2.m2"
fname = "tests/ellipse1_two_sols_HC2.txt"
f = openOut fname
f << "Ellipse1 Two Solutions via Homotopy Continuation (2x2)" << endl;
f << "Target: {-.111, 0, -.309, .889, 0} with points (2,1) or (4,-.6)" << endl;
-- try homotopy continutation
<< "HC2 Ellipse1" << endl;
time for j from 1 to 3 do (
    f << "Trial " << j << endl;
        << "Trial " << j << endl;

            L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
                X1homtime = L#0;
                    X1hom = L#1;
                        f << "time: " << X1homtime << ", value: " << X1hom << endl;
                            << "time: " << X1homtime << ", value: " << X1hom << endl;

                                ABCDEtrue = {-1/8, 0.25, -1/8, -sqrt(2)*0.5, -sqrt(2)*0.5};
                                    ABCDEapprox = specialize(Y, inputValueTable {x => X1hom#0, y => X1hom#1}); 
                                        twonormdiff2 = sqrt fold(plus, (ABCDEapprox - ABCDEtrue)/(i -> i*i));

                                            X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
                                                twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*i));
                                                    if (twonormdiff1 < 0.06) then (
                                                            f << "-- F is close to zero at X1hom: " << X1hom << endl;
                                                                );

                                                                    if (twonormdiff2 < 0.06) then (
                                                                            f << "!!!!!!!! Close to true conic coefficients: " << X1hom << endl;
                                                                                );
                                                                                    f << "----------------------------------------" << endl;
                                                                                    )
                                                                                    f << close;


restart
load "conics/parabola_two_sols_NM.m2"
fname = "tests/parabola_two_sols_NM.txt"
f = openOut fname
f << "Parabola Two Solutions via Newton's Method" << endl;
f << "Target: {-.125, .25, -.125, -.7071, -.7071} with points (-2.82843, 1) or (1.41421, 0.5)" << endl;
-- try homotopy continutation
-- try Newton, expect (-2sqrt(2), 1) or (sqrt(2), 1/2)
-- ie: (-2.82843, 1) or (1.41421, 0.5)
-- newtonsMethod(F, {-2.82843, 1})
-- newtonsMethod(F, {1.41421, 0.5}) -- getting -34.033, .4275 :(
<< "Newton's Method Parabola" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;
    x0 = random (-2*sqrt(2)-0.1, sqrt(2)+0.1);
    y0 = random (0.4, 1.1);
    f << "Starting at: " << (x0, y0) << endl;
    << "Starting at: " << (x0, y0) << endl;
    L = elapsedTiming (newtonsMethod(F, {x0, y0}));
    X1newtime = L#0;
    X1new = L#1;
    f << "time: " << X1newtime << ", value: " << X1new << endl;
    vT = specialize(F, inputValueTable {x => X1new#0, y => X1new#1});
    if (sqrt(vT#0*vT#0 + vT#1*vT#1) < 0.06) then (
        f << "!!!!!!!! Found close enough solution: " << X1new << endl;
    );
)
f << close;

restart
load "conics/parabola_two_sols_HC.m2"
fname = "tests/parabola_two_sols_HC.txt"
f = openOut fname
f << "Parabola Two Solutions via Homotopy Continuation" << endl;
f << "Target: {-.125, .25, -.125, -.7071, -.7071} with points (-2.82843, 1) or (1.41421, 0.5)" << endl;
-- try homotopy continutation
<< "HC Parabola" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {-1/8, 0.25, -1/8, -sqrt(2)*0.5, -sqrt(2)*0.5};
    ABCDEapprox = toList (0..4)/(i -> X1hom#i); 
    twonormdiff2 = sqrt fold(plus, (ABCDEapprox - ABCDEtrue)/(i -> i*i));

    X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
    twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*i));
    if (twonormdiff1 < 0.06) then (
        f << "-- F is close to zero at X1hom: " << X1hom << endl;
    );
    if (twonormdiff2 < 0.06) then (
        f << "-- !!!!!!!! Actually close to true conic coefficients: " << X1hom << endl;
    );
    f << "----------------------------------------" << endl;
)
f << close;


restart
load "conics/parabola_two_sols_HC2.m2"
fname = "tests/parabola_two_sols_HC2.txt"
f = openOut fname
f << "Parabola Two Solutions via Homotopy Continuation (2x2)" << endl;
f << "Target: {-.125, .25, -.125, -.707107, -.707107} with points (-2.82843, 1) or (1.41421, 0.5)" << endl;
-- try homotopy continutation
<< "HC2 Parabola" << endl;
time for j from 1 to 3 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {-1/8, 0.25, -1/8, -sqrt(2)*0.5, -sqrt(2)*0.5};
    ABCDEapprox = specialize(Y, inputValueTable {x => X1hom#0, y => X1hom#1}); 
    twonormdiff2 = sqrt fold(plus, (ABCDEapprox - ABCDEtrue)/(i -> i*i));

    X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
    twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*i));
    if (twonormdiff1 < 0.06) then (
        f << "-- F is close to zero at X1hom: " << X1hom << endl;
    );

    if (twonormdiff2 < 0.06) then (
        f << "!!!!!!!! Close to true conic coefficients: " << X1hom << endl;
    );
    f << "----------------------------------------" << endl;
)
f << close;
-- intervals?
--newtonsMethod(F, {interval[-1.1,-0.99],interval[-3.1,-2.99]})


restart
load "conics/hyperbola_two_sols_NM.m2"
fname = "tests/hyperbola_two_sols_NM.txt"
f = openOut fname
f << "Hyperbola Two Solutions via Newton's Method" << endl;
f << "Target:  {.2, 0, -.16, -1.2, 0} with points (3, 1.1180) or (3.5, 1.7321) " << endl;
-- try Newton
-- newtonsMethod(F, {3, 1.1180}) -- close ish
-- newtonsMethod(F, {3.5, 1.7321}) -- not good
<< "Newton's Method Hyperbola" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;
    x0 = random (2.9, 3.6);
    y0 = random (1.0180, 1.8321);
    f << "Starting at: " << (x0, y0) << endl;
    L = elapsedTiming (newtonsMethod(F, {x0, y0}));
    X1newtime = L#0;
    X1new = L#1;
    f << "time: " << X1newtime << ", value: " << X1new << endl;
    << "time: " << X1newtime << ", value: " << X1new << endl;
    vT = specialize(F, inputValueTable {x => X1new#0, y => X1new#1});
    if (sqrt(vT#0*vT#0 + vT#1*vT#1) < 0.06) then (
        f << "!!!!!!!! Found close enough solution: " << X1new << endl;
    );
)
f << close;


restart
load "conics/hyperbola_two_sols_HC.m2"
fname = "tests/hyperbola_two_sols_HC.txt"
f = openOut fname
f << "Hyperbola Two Solutions via Homotopy Continuation" << endl;
f << "Target:  {.2, 0, -.16, -1.2, 0} with points (3, 1.1180) or (3.5, 1.7321) " << endl;
-- try homotopy continutation
<< "HC Hyperbola" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {.2, 0, -.16, -1.2, 0};
    ABCDEapprox = toList (0..4)/(i -> X1hom#i); 
    twonormdiff2 = sqrt fold(plus, (ABCDEapprox - ABCDEtrue)/(i -> i*i));

    X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
    twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*i));
    if (twonormdiff1 < 0.06) then (
        f << "-- F is close to zero at X1hom: " << X1hom << endl;
    );
    if (twonormdiff2 < 0.06) then (
        f << "-- !!!!!!!! Actually close to true conic coefficients: " << X1hom << endl;
    );
    f << "----------------------------------------" << endl;
)
f << close;


restart
load "conics/hyperbola_two_sols_HC2.m2"
fname = "tests/hyperbola_two_sols_HC2.txt"
f = openOut fname
f << "Hyperbola Two Solutions via Homotopy Continuation (2x2)" << endl;
f << "Target:  {.2, 0, -.16, -1.2, 0} with points (3, 1.1180) or (3.5, 1.7321) " << endl;
-- try homotopy continutation
<< "HC2 Hyperbola" << endl;
time for j from 1 to 3 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {.2, 0, -.16, -1.2, 0};
    ABCDEapprox = specialize(Y, inputValueTable {x => X1hom#0, y => X1hom#1}); 
    twonormdiff2 = sqrt fold(plus, (ABCDEapprox - ABCDEtrue)/(i -> i*i));

    X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
    twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*i));
    if (twonormdiff1 < 0.06) then (
        f << "-- F is close to zero at X1hom: " << X1hom << endl;
    );

    if (twonormdiff2 < 0.06) then (
        f << "!!!!!!!! Close to true conic coefficients: " << X1hom << endl;
    );
    f << "----------------------------------------" << endl;
)
f << close;
-- intervals?
--newtonsMethod(F, {interval[-1.1,-0.99],interval[-3.1,-2.99]})



