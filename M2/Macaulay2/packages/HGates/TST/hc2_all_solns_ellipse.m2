restart
load "conics/ellipse1_two_sols_HC2.m2"
for s in 0..9 do (
    fname = "tests/ellipse_all_sols_HC2_" | toString s | ".txt";
    f = openOut fname;
    
    f << "Ellipse: Tracking All Solutions via Homotopy Continuation  2" << endl;
    f << "Target: (2,1) or (4,-.6)" << endl;
    f << "Time-step d = " << d << endl;
    -- try homotopy continutation
    -- try homotopy continutation
    << "HC2 Ellipse: " << s << endl;
    -- 1 trial for 112 starting points
    startingPointIdx = 0;
    -- A, B = 1, C, x, y = 3rd roots of unity, D, E = +/- 1
    count_solns = 0;
    time for xi in 0..11 do (
        for yi in 0..11 do ( 
            rootx = rootOfUnity(xi, 12);
            rooty = rootOfUnity(yi, 12);
            Gsol = {rootx, rooty};
            f << "Starting point: " << startingPointIdx << ", Gsol: " << Gsol << endl;
            --<< "Starting point: " << startingPointIdx << ", Gsol: " << Gsol << endl;

        

            L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
            X1homtime = L#0;
            X1hom = L#1;
            f << "time: " << X1homtime << ", value: " << X1hom << endl;
            --<< "time: " << X1homtime << ", value: " << X1hom << endl;

            X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
            twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*(conjugate i)));
            if (twonormdiff1 < 0.06) then (
                f << "-- F is close to zero at X1hom: " << X1hom << endl;
                count_solns = count_solns + 1;
            );

            f << "----------------------------------------" << endl;
            startingPointIdx = startingPointIdx + 1;

        )
    )
    << "Total solutions found: " << count_solns << endl;
    f << close;
)
