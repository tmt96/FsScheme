namespace Library

module Repl =
    open System

    open LispVal
    open Parser
    open Errors
    open Eval

    let defaultPrompt = "scheme>> "
    
    let printStr (s: string) = Console.Write(s)
    let printStrLine (s: string) = Console.WriteLine(s)
    let printNewLine () = Console.WriteLine()


    let readPrompt () =
        printStr defaultPrompt
        Console.ReadLine()

    let evalString expr =
        try
            expr |> ReadExpr |> Eval
        with
        | LispException(error) -> String (ShowError error)

    let evalAndPrint =
        evalString >> ShowVal >> printStr >> printNewLine
    
    let rec until pred (prompt: unit -> string) evaluator =
        let result = prompt ()
        if not (pred result) then
            evaluator result
            until pred prompt evaluator

    let runRepl =
        let terminator = fun (s: string) -> s.ToLower() = "quit"
        until terminator readPrompt evalAndPrint 

    let runFile filename args = ()
    