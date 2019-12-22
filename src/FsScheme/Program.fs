// Learn more about F# at http://fsharp.org

open System

open Library.Repl

let rec fib n: int64 =
    if n <= 1 then int64 1 else fib (n-1) + fib (n-2)

[<EntryPoint>]
let main argv =
    match Array.toList argv with
    | [] -> runRepl
    | filename :: args -> runOne filename args
    0
