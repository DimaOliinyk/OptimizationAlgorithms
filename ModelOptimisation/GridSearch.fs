module Search

open Optimization

    let GridSearch (pointsCount: int) (config: OptimizationConfig): OptimizationResult = 
        [1..(pointsCount - 1)] 
        |> 
        Seq.fold (fun (bestRes: OptimizationResult) i ->
            let step = (config.UpperBound - config.LowerBound) / ((double)pointsCount - 1.)
            let currP = config.LowerBound + double(i) * step
            let currV = currP |> config.Function 
            { 
                X = if currV < bestRes.Extremum then currP else bestRes.X;
                Extremum = if currV < bestRes.Extremum then currV else bestRes.Extremum; 
                IterationsCount = bestRes.IterationsCount + 1.
            }) 

            {   
                X = config.LowerBound; 
                Extremum = config.LowerBound |> config.Function; 
                IterationsCount = 0
            }
            

    let rec DichtomySearch (config: OptimizationConfig) (iterationsCount: double): OptimizationResult = 
        if ((config.UpperBound - config.LowerBound) / 2.) > config.Tolerance && iterationsCount < 100 then
            let mid = (config.UpperBound + config.LowerBound) / 2.
            let left = mid - config.Tolerance / 2. 
            let right = mid + config.Tolerance / 2. 
            
            DichtomySearch ({ 
                UpperBound = if (config.Function left) < (config.Function right) then right else config.UpperBound;
                LowerBound = if (config.Function left) > (config.Function right) then left else config.LowerBound;
                Function = config.Function;
                Tolerance = config.Tolerance;    
            }) (iterationsCount + 1.) 
        else 
            {
                X = (config.UpperBound + config.LowerBound) / 2.;
                Extremum = config.Function ((config.UpperBound + config.LowerBound) / 2.);
                IterationsCount = iterationsCount
            }
