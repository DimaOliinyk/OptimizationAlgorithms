
module CoordinateDescentMethod
    open Search 

    let CoordinateDescent (initialStep: double) (tolerence: double) (func: double array -> double) (initialPoint: double array) = 
        let maxIter = 1000

        let dim = Seq.length initialPoint
        let mutable currP = Array.copy initialPoint
        let mutable step = initialStep
        let mutable bestRes = func initialPoint
        let mutable bestPoint = initialPoint

        for i in 1..maxIter-1 do
            step <- 100
            currP
            |> Array.mapi (fun i x -> 
               bestRes <- func currP
               let res = GridSearch 100 { 
                    LowerBound = x - step; 
                    UpperBound = x + step;
                    Tolerance = tolerence;
                    Function = (fun x -> 
                        func (Array.init dim (fun j -> 
                            if j = i then x else currP.[j])))}
               
               let newP = Array.init dim (fun j -> 
                    if i = j then res.X else currP.[j])

               if func newP < bestRes then 
                    bestPoint <- newP 
                    bestRes <- func newP
               step <- step / 2.
               currP <- bestPoint
            ) 

            |> ignore
            
        bestPoint

