// Learn more about F# at http://fsharp.org

open System
open Library.Parser
open Library.Eval
open Library.Errors
open Library.LispVal

let rec fib n: int64 =
    if n <= 1 then int64 1 else fib (n-1) + fib (n-2)

let evalString expr =
    try
        expr |> ReadExpr |> Eval
    with
    | LispException(error) -> String (ShowError error)
    
[<EntryPoint>]
let main argv =
    let repl = evalString >> ShowVal
    Console.WriteLine(repl argv.[0])
    0
