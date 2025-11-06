restart
load "conics/conic_1.m2"
fname = "tests/1conic1_HC.txt"
f = openOut fname
f << "Conic 1 Two Solutions via Homotopy Continuation" << endl;
f << "Target: -- {-0.750, 0.083, -0.819, -0.595, -0.280, 0.3274, -1.108}" << endl;
f << "or {-0.750, 0.083, -0.819, -0.595, -0.280, 0.3503, 0.7835}" << endl;
-- try homotopy continutation
<< "HC Conic 1" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {-0.750, 0.083, -0.819, -0.595, -0.280};
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
load "conics/conic_2.m2"
fname = "tests/1conic2_HC.txt"
f = openOut fname
f << "Conic 2 Two Solutions via Homotopy Continuation" << endl;
f << "Target: {-0.630, -0.191, -0.598, 0.567, -0.672, 0.108, -2.0281}
 or {-0.630, -0.191, -0.598, 0.567, -0.672, 1.0544, -2.1567}" << endl;
-- try homotopy continutation
<< "HC Conic 2" << endl;
time for j from 1 to 100 do (
    f << "Trial " << j << endl;
    << "Trial " << j << endl;

    L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
    X1homtime = L#0;
    X1hom = L#1;
    f << "time: " << X1homtime << ", value: " << X1hom << endl;
    << "time: " << X1homtime << ", value: " << X1hom << endl;

    ABCDEtrue = {-0.630, -0.191, -0.598, 0.567, -0.672};
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
load "conics/conic_3.m2"
fname = "tests/1conic3_HC.txt"
f = openOut fname
f << "Conic 3 Two Solutions via Homotopy Continuation" << endl;
f << "Target: {-0.793, -0.038, -0.773, -0.214, 0.351, 0.7407, -0.5457}
    or  {-0.793, -0.038, -0.773, -0.214, 0.351, -0.1365, 1.3984}" << endl;
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

