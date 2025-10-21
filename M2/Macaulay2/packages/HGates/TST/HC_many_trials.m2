restart
load "conics/conic_3.m2"
fname = "tests/conic3_HC.txt"
f = openOut fname
f << "Conic 3 Two Solutions via Homotopy Continuation" << endl;
f << "Target: {-0.793, -0.038, -0.773, -0.214, 0.351, 0.7407, -0.5457}
    or {-0.793, -0.038, -0.773, -0.214, 0.351, -0.1365, 1.3984}" << endl;
-- try homotopy continutation
<< "HC Conic 3" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {-0.793, -0.038, -0.773, -0.214, 0.351};
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
);
f << close;

restart
load "conics/conic_7.m2"
fname = "tests/conic7_HC.txt"
f = openOut fname
f << "Conic 7 Two Solutions via Homotopy Continuation" << endl;
f << "Target: {-0.581, -0.098, -0.438, 0.247, -0.794, -0.8493, -2.0366}
 or {-0.581, -0.098, -0.438, 0.247, -0.794, 0.5778, -2.733}" << endl;
-- try homotopy continutation
<< "HC Conic 7" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {-0.581, -0.098, -0.438, 0.247, -0.794};
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
);
f << close;

restart
load "conics/conic_5.m2"
fname = "tests/conic5_HC.txt"
f = openOut fname
f << "Conic 5 Two Solutions via Homotopy Continuation" << endl;
f << "Target: {-0.308, -0.131, -0.516, 0.952, -0.275, 3.6631, -1.8324}
    or  {-0.308, -0.131, -0.516, 0.952, -0.275, 2.0828, -2.3941}" << endl;
-- try homotopy continutation
<< "HC Conic 5" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {-0.308, -0.131, -0.516, 0.952, -0.275};
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
);
f << close;

restart
load "conics/conic_6.m2"
fname = "tests/conic6_HC.txt"
f = openOut fname
f << "Conic 6 Two Solutions via Homotopy Continuation" << endl;
f << "Target: {-0.782, 0.049, -0.730, 0.199, 0.496, -0.9011, 0.9002}
-- {-0.782, 0.049, -0.730, 0.199, 0.496, 0.8121, -0.6426}" << endl;
-- try homotopy continutation
<< "HC Conic 6" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {-0.782, 0.049, -0.730, 0.199, 0.496};
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
);
f << close;

