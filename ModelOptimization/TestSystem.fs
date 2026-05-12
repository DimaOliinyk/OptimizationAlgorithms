module TestSystem

    open System
    open System.Collections.Generic


    let Gain (gainCoefficient: double) (x: double) = 
        gainCoefficient * x 

    let Integrate (dt: double) = 
        let mutable prev: double = 0.
        let mutable sum: double = 0.
        fun (x: double) -> 
            sum <- (prev + x) * dt / 2. + sum
            prev <- x 
            sum

    let Derive (dt: double)  = 
        let mutable prev = 0.
        fun (x: double) -> 
            let y = (x - prev) / dt 
            prev <- x 
            y

    let Aperiodic (dt: double) (timeConstant: double) = 
        let mutable prev = 0.
        fun (x: double) -> 
            let y = (dt * x + timeConstant * prev) / (timeConstant + dt)
            prev <- y 
            y

    let Delay (dt: double) (timeDelay: double) = 
        let delayQueue = Queue<double>()
        let count = int (timeDelay / dt)

        if count < 1 then None
        else Some(
            fun (x: double) ->
                delayQueue.Enqueue(x)
                if delayQueue.Count > count then
                    delayQueue.Dequeue()
                else 
                    0)

    let IntegrateAndLimit (dt: double) (minLimit: double Option) (maxLimit: double Option) = 
        let mutable prev: double = 0.
        let mutable sum: double = 0.

        fun (x: double) -> 
            sum <- (prev + x) * dt / 2. + sum
            prev <- x 
            
            match (minLimit, sum) with 
            | Some(lowlim), s when s <= lowlim -> sum <- lowlim
            | _ -> ()
                                    
            match (maxLimit, sum) with 
            | Some(uplim), s when s >= uplim -> sum <- uplim
            | _ -> ()
            
            sum



    let PID (dt: double) (pGain: double) (iGain: double) (dGain: double) =
        let intg = Integrate dt
        let derv = Derive dt
        
        fun (x: double) ->  
            Gain pGain x + 
            Gain dGain (derv x) + 
            Gain iGain (intg x) 
    

    type PIDparams = { Pgain: double; Igain: double; Dgain: double }

    let Model (dt: double) = 
        let intg1 = IntegrateAndLimit dt None None 
        let intg2 = IntegrateAndLimit dt None None

        fun (x: double) -> 
            x
            |> Gain(Math.PI / 180.0)
            |> sin
            |> Gain 3.36
            |> intg1
            |> intg2

    let SetUpController (setpoint: double) (dt: double) (pid: PIDparams) = 
        let contr = PID dt pid.Pgain pid.Igain pid.Dgain     

        fun (x: double) ->
            (setpoint - x)
            |> contr


    let SquareTimeError (dt: double) (alpha: double) = 
        let mutable time = 0.0

        fun (e: double) -> 
            time <- time + dt
            ((e ** 2.0) * (time ** alpha))
        

    let StepResponseClosedLoop (pidParams: PIDparams) (setpoint: float) = 
        let dt = 0.25
        let contr = SetUpController setpoint dt pidParams 
        let modl = Model dt

        Seq.unfold (fun state -> Some (state, 
            state |> modl |> contr)) 0.

