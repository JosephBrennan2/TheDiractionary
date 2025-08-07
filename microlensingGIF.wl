(* Inspired by: https://pressbooks.cuny.edu/astrobiology/chapter/transit-astrometry-and-gravitational-microlensing-techniques/ *)
createMicrolensingGIF[] :=
 Module[
    {u, paczynskiModel, planetEffect, sourceBrightness},
    u[t_, t0_, tE_, u0_] := Sqrt[u0^2 + ((t - t0)/tE)^2]; 
    paczynskiModel[t_, t0_, tE_, u0_] := ((u[t, t0, tE, u0]^2 + 2)/(u[t, t0, tE, u0]*Sqrt[u[t, t0, tE, u0]^2 + 4]));
    planetEffect[t_] := If[Abs[t - 10] < 1, 1/(Abs[t - 10] + 0.1), 1];
    sourceBrightness[t_] := paczynskiModel[t, 0, 30, 0.1] + planetEffect[t];
    Manipulate[
        Column[
            {
                Graphics[
                    Table[
                        If[
                            time > t,
                            {
                                ColorData["SolarColors"][sourceBrightness[t]/10],
                                Disk[{t, 0}, 1]
                            }
                        ],
                        {t, -30, time, 2}
                    ],
                    PlotRange -> {{-30, 30}, {-1, 1}}, Axes -> False, ImagePadding -> {{50, 10}, {0, 10}}, ImageSize -> Large
                ],
                Plot[
                    paczynskiModel[t, 0, 30, 0.1] + planetEffect[t] - 1,
                    {t, -30, time},
                    PlotStyle -> Black, Frame -> True, FrameLabel -> {"time (in days)", "magnification"},
                    PlotRange -> {{-30, 30}, {0, 12}}, ImagePadding -> {{50, 10}, {35, 10}}, ImageSize -> Large,
                    Epilog -> 
                    {
                        If[ 
                            time > -10,
                            Text[Style["Magnification by \n stellar lens", 14], {-10, 8}]
                        ],
                        If[ 
                            time > 10,
                            Text[Style["Deviation \ndue to \nplanet", 14], {15, 8}]
                        ]
                    }
                ]
            }
        ],
        {{time, -29, "Time"}, -29, 30},
        ContinuousAction -> True, SaveDefinitions -> True
    ]
];