module TestFunctions
    
    let Rosenbrock (x: double seq) = 
        if Seq.length x < 2 then
            None
        else 
            Some ((Seq.map2 (
                    fun a b -> 
                       double(100.*(b - a**2.)**2. + 
                       (1. - a)**2.)
                  ) x (Seq.skip 1 x))
                  |> Seq.sum )


    let Powell (x: double seq) = 
        let everyForth items =
            items
            |> Seq.indexed
            |> Seq.filter (fun (i, _) -> (i + 1) % 4 = 0)
            |> Seq.map snd

        if Seq.length x % 4 <> 0 then
            None
        else 
            Some (
                Seq.zip (Seq.zip3 
                    (x |> everyForth)
                    (x |> Seq.skip 1 |> everyForth)
                    (x |> Seq.skip 2 |> everyForth))
                    (x |> Seq.skip 3 |> everyForth)
                |> Seq.map
                    (fun ((a, b, c), d) -> (a, b, c, d))
                |> Seq.map 
                    (fun (a, b, c, d) -> 
                        (a + 10.*b)**2. + 
                        5.*(c - d)**2. + 
                        (b - 2.*c)**4. +
                        10.*(a-d)**4.
                    )
                |> Seq.sum
            )


    let Zangwill2 (x: double seq) =
        if Seq.length x < 2 then 
            None 
        else 
            Some ( 
                (Seq.map2 (
                    fun a b -> 
                       (a**2. + b**2. + a*b)**2.
                ) x (Seq.skip 1 x))
                |> Seq.sum
            )


    let myFunc (x: double seq) =
        if Seq.length x < 2 then 
            None 
        else 
            Some ( 
                (Seq.map2 (
                    fun a b -> 
                        double(a**2. - a*b + b**2. - 2.0*a + 3.0*b - 4.)
                ) x (Seq.skip 1 x))
                |> Seq.sum
            )