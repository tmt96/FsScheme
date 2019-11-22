// Learn more about F# at http://fsharp.org

open System

open Library.Repl

let rec fib n: int64 =
    if n <= 1 then int64 1 else fib (n-1) + fib (n-2)

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 0 -> runRepl
    | 1 -> argv |> Array.toList |> runOne 
    | otherwise -> Console.WriteLine("Program expects 0 or 1 arguments, received " + (string otherwise))
    0
