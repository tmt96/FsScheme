namespace Library

module Repl =
    open System

    open LispVal
    open Parser
    open Errors
    open SymbolTable
    open Eval

    let defaultPrompt = "scheme>> "
    
    let printStr (s: string) = Console.Write(s)
    let printStrLine (s: string) = Console.WriteLine(s)
    let printNewLine () = Console.WriteLine()


    let readPrompt () =
        printStr defaultPrompt
        Console.ReadLine()

    let evalString env expr =
        try
            expr |> readExpr |> eval env
        with
        | LispException(error) -> String (ShowError error)

    let evalAndPrint env =
        evalString env >> ShowVal >> printStr >> printNewLine
    
    let rec until pred (prompt: unit -> string) evaluator =
        let result = prompt ()
        if not (pred result) then
            evaluator result
            until pred prompt evaluator

    let runRepl =
        let env = nullEnv ()
        let terminator = fun (s: string) -> s.ToLower() = "quit"
        until terminator readPrompt (evalAndPrint env) 

    let runOne expr =
        evalAndPrint (nullEnv()) expr 

    let runFile filename args = ()
    