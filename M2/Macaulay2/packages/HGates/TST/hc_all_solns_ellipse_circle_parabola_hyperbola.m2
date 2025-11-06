restart
load "conics/ellipse_all_sols_HC.m2"
for s in 0..1 do (
    fname = "finaltests/ellipse_all_sols_HC_" | toString s | ".txt";
    f = openOut fname;
    
    f << "Ellipse: Tracking All Solutions via Homotopy Continuation" << endl;
    f << "Target: {-.111, 0, -.309, .889, 0} with points (2,1) or (4,-.6)" << endl;
    f << "Time-step d = " << d << endl;
    -- try homotopy continutation
    -- try homotopy continutation
    << "HC Ellipse: " << s << endl;
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

                        L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
                        X1homtime = L#0;
                        pcResults = L#1;
                        X1homInitial = pcResults#0;
                        X1hom = newtonsMethod(MF, X1homInitial, iterations => 50);

                        condJF = pcResults#1;
                        f << "time: " << X1homtime << ", value: " << X1hom << endl;
                        --<< "time: " << X1homtime << ", value: " << X1hom << endl;

                        X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
                        twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*(conjugate i)));
                        
                        if (twonormdiff1 < 1.0e-6) then (
                            << startingPointIdx << "diff: " << twonormdiff1 << endl;
                            if (condJF < 1.0e3) then (
                                f << "-- F is close to zero at X1hom with good condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                            ) else (
                                f << "-- F is close to zero at X1hom but with poor condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                                << "-- F is close to zero at X1hom but with poor condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                            );
                            count_solns = count_solns + 1;
                        );
                        
                        f << "----------------------------------------" << endl;
                        startingPointIdx = startingPointIdx + 1;
                        
                    )
                )
            )
        )
    )
    << "Total solutions found: " << count_solns << endl;
    f << close;
)

restart
load "conics/circle_all_sols_HC.m2"
for s in 0..1 do (
    fname = "finaltests/circle_all_sols_HC_" | toString s | ".txt";
    f = openOut fname;

    f << "Circle: Tracking All Solutions via Homotopy Continuation" << endl;
    f << "Target: Target: {-.25, 0, -.25, 0, 0} with 4 points (+/-1.414, +/1.414)" << endl;
    f << "Time-step d = " << d << endl;
    -- try homotopy continutation
    -- try homotopy continutation
    << "HC Circle: " << s << endl;
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

                        L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
                        X1homtime = L#0;
                        pcResults = L#1;
                        X1homInitial = pcResults#0;
                        X1hom = newtonsMethod(MF, X1homInitial, iterations => 50);

                        condJF = pcResults#1;
                        f << "time: " << X1homtime << ", value: " << X1hom << endl;
                        --<< "time: " << X1homtime << ", value: " << X1hom << endl;

                        X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
                        twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*(conjugate i)));
                        
                        if (twonormdiff1 < 1.0e-6) then (
                            << startingPointIdx << "diff: " << twonormdiff1 << endl;
                            if (condJF < 1.0e3) then (
                                f << "-- F is close to zero at X1hom with good condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                            ) else (
                                f << "-- F is close to zero at X1hom but with poor condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                                << "-- F is close to zero at X1hom but with poor condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                            );
                            count_solns = count_solns + 1;
                        );
                        
                        f << "----------------------------------------" << endl;
                        startingPointIdx = startingPointIdx + 1;
                        
                    )
                )
            )
        )
    )
    << "Total solutions found: " << count_solns << endl;
    f << close;
)

restart
load "conics/hyperbola_all_sols_HC.m2"
for s in 0..1 do (
    fname = "finaltests/hyperbola_all_sols_HC__" | toString s | ".txt";
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

                        L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
                        X1homtime = L#0;
                        pcResults = L#1;
                        X1homInitial = pcResults#0;
                        X1hom = newtonsMethod(MF, X1homInitial, iterations => 50);

                        condJF = pcResults#1;
                        f << "time: " << X1homtime << ", value: " << X1hom << endl;
                        --<< "time: " << X1homtime << ", value: " << X1hom << endl;

                        X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
                        twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*(conjugate i)));
                        
                        if (twonormdiff1 < 1.0e-6) then (
                            << startingPointIdx << "diff: " << twonormdiff1 << endl;
                            if (condJF < 1.0e3) then (
                                f << "-- F is close to zero at X1hom with good condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                            ) else (
                                f << "-- F is close to zero at X1hom but with poor condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                                << "-- F is close to zero at X1hom but with poor condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                            );
                            count_solns = count_solns + 1;
                        );
                        
                        f << "----------------------------------------" << endl;
                        startingPointIdx = startingPointIdx + 1;
                        
                    )
                )
            )
        )
    )
    << "Total solutions found: " << count_solns << endl;
    f << close;
)

restart
load "conics/parabola_all_sols_HC.m2"
for s in 0..1 do (
    fname = "finaltests/parabola_all_sols_HC_second" | toString s | ".txt";
    f = openOut fname;

    f << "Parabola: Tracking All Solutions via Homotopy Continuation" << endl;
    f << "Target: {-.125, .25, -.125, -.707107, -.707107} with points (-2.82843, 1) or (1.41421, 0.5)" << endl;
    f << "Time-step d = " << d << endl;
    -- try homotopy continutation
    -- try homotopy continutation
    << "HC Parabola: " << s << endl;
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

                        L = elapsedTiming (predictorCorrector(MF, MG, Gsol, d, fileName => f));
                        X1homtime = L#0;
                        pcResults = L#1;
                        X1homInitial = pcResults#0;
                        X1hom = newtonsMethod(MF, X1homInitial, iterations => 50);

                        condJF = pcResults#1;
                        f << "time: " << X1homtime << ", value: " << X1hom << endl;
                        --<< "time: " << X1homtime << ", value: " << X1hom << endl;

                        X1vT = inputValueTable (toList (0..(length X - 1))/(i -> X.Elements#i => X1hom#i));
                        twonormdiff1 = sqrt fold(plus, (specialize(F, X1vT))/(i -> i*(conjugate i)));
                        
                        if (twonormdiff1 < 1.0e-6) then (
                            << startingPointIdx << "diff: " << twonormdiff1 << endl;
                            if (condJF < 1.0e3) then (
                                f << "-- F is close to zero at X1hom with good condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                            ) else (
                                f << "-- F is close to zero at X1hom but with poor condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                                << "-- F is close to zero at X1hom but with poor condition number: " << X1hom << ", cond(J_F): " << condJF << endl;
                            );
                            count_solns = count_solns + 1;
                        );
                        
                        f << "----------------------------------------" << endl;
                        startingPointIdx = startingPointIdx + 1;
                        
                    )
                )
            )
        )
    )
    << "Total solutions found: " << count_solns << endl;
    f << close;
)
