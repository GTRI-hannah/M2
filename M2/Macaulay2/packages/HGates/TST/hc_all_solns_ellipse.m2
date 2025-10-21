restart
load "conics/ellipse_all_sols_HC.m2"
fname = "tests/ellipse_all_sols_HC_second.txt"
f = openOut fname
f << "Ellipse: Tracking All Solutions via Homotopy Continuation" << endl;
f << "Target: {-.111, 0, -.309, .889, 0} with points (2,1) or (4,-.6)" << endl;
-- try homotopy continutation
-- try homotopy continutation
<< "HC Ellipse" << endl;
-- 20 trials for 108 starting points
startingPointIdx = 0;
-- A, B = 1, C, x, y = 3rd roots of unity, D, E = +/- 1
for xi in 0..2 do (
    for yi in 2..2 do ( 
        for Ci in 0..2 do (
            for Di in {-1, 1} do (
                for Ei in {-1, 1} do (
                    rootx = rootOfUnity(xi, 3);
                    rooty = rootOfUnity(yi, 3);
                    rootC = rootOfUnity(Ci, 3);
                    Gsol = {1, 1, rootC, rootx, rooty, Di, Ei};
                    f << "Starting point: " << startingPointIdx << ", Gsol: " << Gsol << endl;
                    << "Starting point: " << startingPointIdx << ", Gsol: " << Gsol << endl;

                    time for j from 1 to 20 do (
                        f << "Trial " << j << endl;
                        << "Trial " << j << endl;

                        L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
                        X1homtime = L#0;
                        X1hom = L#1;
                        f << "time: " << X1homtime << ", value: " << X1hom << endl;
                        << "time: " << X1homtime << ", value: " << X1hom << endl;

                        ABCDEtrue = {-.111, 0, -.309, .889, 0};
                        ABCDEapprox = toList (0..4)/(i -> X1hom#i); 
                        twonormdiff2 = sqrt fold(plus, (ABCDEapprox - ABCDEtrue)/(i -> i*i));

                        X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
                        twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*i));
                        if (twonormdiff1 < 0.06) then (
                            f << "-- F is close to zero at X1hom: " << X1hom << endl;
                        );
                        if (twonormdiff2 < 0.06) then (
                            "-- Close to true conic coeffis: " << X1hom << endl;
                        );
                        f << "----------------------------------------" << endl;
                        startingPointIdx = startingPointIdx + 1;
                    )
                )
            )
        )
    )
)

f << close;