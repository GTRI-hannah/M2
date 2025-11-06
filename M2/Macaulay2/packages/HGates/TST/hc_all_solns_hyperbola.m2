restart
load "conics/hyperbola_all_sols_HC.m2"
for s in 0..9 do (
    fname = "tests/hyperbola_all_sols_HC_second" | toString s | ".txt";
    f = openOut fname;

    f << "Hyperbola: Tracking All Solutions via Homotopy Continuation" << endl;
    f << "Target: {.2, 0, -.16, -1.2, 0} with points (3, 1.1180) or (3.5, 1.7321)" << endl;
    f << "Time-step d = " << d << endl;
    -- try homotopy continutation
    -- try homotopy continutation
    << "HC Hyperbola: " << s << endl;
    -- 1 trial for 108 starting points
    startingPointIdx = 0;
    -- A, B = 1, C, x, y = 3rd roots of unity, D, E = +/- 1
    count_solns = 0;
    time for xi in 0..2 do (
        for yi in 0..2 do ( 
            for Ci in 0..2 do (
                for Di in {-1, 1} do (
                    for Ei in {-1, 1} do (
                        rootx = rootOfUnity(xi, 3);
                        rooty = rootOfUnity(yi, 3);
                        rootC = rootOfUnity(Ci, 3);
                        Gsol = {1, 1, rootC, rootx, rooty, Di, Ei};
                        f << "Starting point: " << startingPointIdx << ", Gsol: " << Gsol << endl;
                        --<< "Starting point: " << startingPointIdx << ", Gsol: " << Gsol << endl;

                        for j from 1 to 1 do (
                            f << "Trial " << j << endl;
                            --<< "Trial " << j << endl;

                            L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
                            X1homtime = L#0;
                            X1hom = L#1;
                            f << "time: " << X1homtime << ", value: " << X1hom << endl;
                            --<< "time: " << X1homtime << ", value: " << X1hom << endl;

                            ABCDEtrue = {-.111, 0, -.309, .889, 0};
                            ABCDEapprox = toList (0..4)/(i -> X1hom#i); 
                            twonormdiff2 = sqrt fold(plus, (ABCDEapprox - ABCDEtrue)/(i -> i*(conjugate i)));

                            X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
                            twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*(conjugate i)));
                            if (twonormdiff1 < 0.06) then (
                                f << "-- F is close to zero at X1hom: " << X1hom << endl;
                                count_solns = count_solns + 1;
                            );
                            if (twonormdiff2 < 0.06) then (
                                f << "-- Close to true conic coeffis: " << X1hom << endl;
                            );
                            f << "----------------------------------------" << endl;
                            startingPointIdx = startingPointIdx + 1;
                        )
                    )
                )
            )
        )
    )
    << "Total solutions found: " << count_solns << endl;
    f << close;
)
