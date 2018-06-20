// Learn more about F# at http://fsharp.org

open System
open Library.Parser
open Library.Eval
open Library.LispVal

let rec fib n: int64 =
    if n <= 1 then int64 1 else fib (n-1) + fib (n-2)
    
[<EntryPoint>]
let main argv =
    let repl = ReadExpr >> Eval >> ShowVal
    Console.WriteLine(repl argv.[0])
    0
