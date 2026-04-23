module HookeJeevesMethod

    let Search (step: double) (func: double array -> double) (initialPoint: double array) = 
        let dim = Seq.length initialPoint
        let mutable trialPt = initialPoint |> Array.copy
        let mutable bestVal = func initialPoint

        [| 0..(dim-1) |] 
        |> Array.iter (fun i -> 
            let trialPos = Array.mapi (fun j x -> if i = j then x + step else x) trialPt
            let trialNeg = Array.mapi (fun j x -> if i = j then x - step else x) trialPt
            
            match (bestVal, func trialPos, func trialNeg) with
            | (best, pos, neg) when pos < best ->
                bestVal <- pos
                trialPt <- trialPos
            | (best, pos, neg) when neg < best ->
                bestVal <- neg
                trialPt <- trialNeg 
            | _ -> () )
        trialPt


    let HookeJeeves (initialStep: double) (tolerence: double) (func: double array -> double) (initialPoint: double array) =
        let dim = Seq.length initialPoint 
        let mutable step = initialStep
        let mutable currPt = Array.copy initialPoint
        let mutable bestPt = Array.copy initialPoint
        let maxIter = 10_000
        let stepReductionFactor = 0.5
        
        [|
            for i in [1..(maxIter - 1)] |> Seq.takeWhile (fun _ -> step > tolerence) do
                let trialPt = Search step func currPt 

                match (func bestPt, func trialPt) with 
                | (best, trial) when trial < best ->  
                    let nxtPt = 
                        Array.zip bestPt trialPt
                        |> Array.map (fun (xb, xt) -> 
                            2.0*xt - xb)
                    currPt <- nxtPt
                    bestPt <- trialPt
                | _ -> 
                    step <- step * stepReductionFactor
                    currPt <- bestPt
                yield bestPt
        |]



       

       //[| 0..(dim-1) |] 
       // |> Array.iter (fun i -> 
       //     let original = trialPt.[i]

       //     trialPt[i] <- original + step
       //     let frwrdVal = func trialPt 
            
       //     if frwrdVal < bestVal then 
       //         bestVal <- frwrdVal
       //     else 
       //         trialPt[i] <- original - step
       //         let bckwrdVal = func trialPt

       //         if bckwrdVal < bestVal then 
       //             bestVal <- bckwrdVal
       //         else 
       //             trialPt[i] <- original