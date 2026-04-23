open Optimization
open Search
open TestFunctions
open NedlerMeadMethod
open HookeJeevesMethod
open CoordinateDescentMethod
open System


(*let conf = {
    LowerBound = -10;
    UpperBound = 10;
    Tolerance = 0.001;
    Function = fun x -> x**2. - 4.*x + 4.+x**3.
}*)

(*let methodRes = GridSearch 100 conf
Console.WriteLine($"Extremum {methodRes.Extremum:F2} at {methodRes.X:F2}; iters: {methodRes.IterationsCount}")

let method2Res = DichtomySearch conf 0
Console.WriteLine($"Extremum {method2Res.Extremum:F2} at {method2Res.X:F2}; iters: {method2Res.IterationsCount}")*)


let ip = [| double(10.0); double(10.0) |]
let tolerance = 1e-10


let testFunc1 (x: double seq) = Rosenbrock x |> Option.get
let testFunc2 (x: double seq) = Zangwill2 x |> Option.get
let myFuncfunc (x: double seq) = myFunc x |> Option.get


Console.WriteLine("\n ===== NelderMead =====\nTests:")
Console.WriteLine("1. Rosenbrock: ")
NedlerMead tolerance testFunc1 ip
|> Seq.rev
|> Seq.take 1
|> Seq.iter (fun (solution, eval) -> 
    printfn "%A: \t %04f" solution eval)

Console.WriteLine("2. Zangwill: ")
NedlerMead tolerance testFunc2 ip
|> Seq.rev
|> Seq.take 1
|> Seq.iter (fun (solution, eval) -> 
    printfn "%A: \t %04f" solution eval)

Console.WriteLine("\nMy Function: ")
NedlerMead tolerance myFuncfunc ip
|> Seq.rev
|> Seq.take 1
|> Seq.iter (fun (solution, eval) -> 
    printfn "%A: \t %04f" solution eval)


(*Console.WriteLine("\n ===== Hooke-Jeeves =====")
HookeJeeves 0.5 tolerance func ip
|> Seq.rev
|> Seq.take 1
|> Seq.iter (fun (solution) -> 
    printfn "%A: \t %.4f" solution (func solution))*)
