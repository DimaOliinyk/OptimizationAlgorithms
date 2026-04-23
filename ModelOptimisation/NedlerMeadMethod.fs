module NedlerMeadMethod

    let Update (func: double array -> double) (simplex: array<double> array) = 
        let ordered = simplex |> Array.sortBy func
        let dim = Array.length simplex.[0]
        let size = Array.length simplex

        let gamma = 2.
        let alpha = 1.
        let rho = 0.5
        let sigma = 0.5

        let bestCandidates = ordered[..size-2] 

        let centroid = 
            Array.init dim (fun x -> 
                bestCandidates
                |> Array.averageBy(fun p -> p[x]))
        
        let worst = ordered[size-1]
                
        let reflected = 
            Array.init dim (fun x ->
                centroid[x] + alpha * (centroid[x] - worst[x]))

        let secondWorst = ordered[size-2]
        let best = ordered[0]

        if (func reflected < func secondWorst) &&
           (func reflected >= func best) 
        then 
            ordered[size-1] <- reflected
            ordered
        elif (func reflected < func best) then
            let expanded = 
                Array.init dim (fun x ->
                    centroid[x] + gamma * (reflected[x] - centroid[x]))
            if func expanded < func reflected then 
                ordered[size-1] <- expanded
            else 
                ordered[size-1] <- reflected
            ordered

        elif (func reflected < func worst) then 
            let contractedOutside = 
                Array.init dim (fun x -> 
                    centroid[x] + rho * (reflected[x] - centroid[x]))
            if func contractedOutside < func reflected then 
                ordered[size - 1] <- contractedOutside 
                ordered
            else
                let shrunk = 
                    ordered
                    |> Array.map (fun p -> 
                        Array.init dim (fun x -> 
                            best[x] + sigma * (p[x] - best[x])))
                shrunk   
        elif func reflected >= func worst then 
            let contractedInside =
                Array.init dim (fun x ->
                    centroid[x] + rho * (worst[x] - centroid[x]))
            if func contractedInside < func worst then 
                ordered[size - 1] <- contractedInside
                ordered
            else
                let shrunk = 
                    ordered
                    |> Array.map (fun p -> 
                        Array.init dim (fun x ->
                            best[x] + sigma * (p[x] - best[x])))

                shrunk
        else
            failwith "All cases covered"

    
    let Terminate (tolerance: double) (func: double array -> double) (simplex: double array array) = 
        (simplex |> Seq.map func |> Seq.max) - (simplex |> Seq.map func |> Seq.min) < tolerance


    let Init (initialPoint: double array) = 
        let dim = Seq.length initialPoint
        [|
            yield initialPoint
            for d in 0..(dim - 1) ->
                let x = initialPoint |> Array.copy
                x[d] <- initialPoint[d] + 1.
                x
            for d in 0..(dim - 1) -> 
                let x = initialPoint |> Array.copy
                x[d] <- initialPoint[d] - 1.
                x
        |]


    let NedlerMead (tolerence: double) (func: double array -> double) (initialPoint: double array) =
        let simplex = Init initialPoint

        simplex
        |> Seq.unfold (fun simplex -> 
            let updatedSimplex = Update func simplex
            let solution = 
                updatedSimplex
                |> Array.map (fun p -> p, func p)
                |> Array.minBy snd
            Some((solution, updatedSimplex), updatedSimplex))
        |> Seq.takeWhile (fun (solution, simplex) ->
            simplex |> Terminate tolerence func |> not)
        |> Seq.map (fun ((solution, eval), _) -> 
            (solution, eval))
