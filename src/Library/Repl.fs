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
        | LispException(error) -> String (string error)

    let evalAndPrint env =
        evalString env >> string >> printStr >> printNewLine
    
    let rec until pred (prompt: unit -> string) evaluator =
        let result = prompt ()
        if not (pred result) then
            evaluator result
            until pred prompt evaluator


    let loadStdLib env =
        eval env
            (List
                [ Atom "load"
                  sprintf "%s/../../scheme_stdlib/stdlib.scm" __SOURCE_DIRECTORY__ |> String ])
        |> ignore
        env

    let runRepl =
        let env = primitiveBindings() |> loadStdLib
        let terminator = fun (s: string) -> s.ToLower() = "quit"
        until terminator readPrompt (evalAndPrint env) 

    let runOne filename args =
        let env =
            primitiveBindings()
            |> loadStdLib
            |> bindVars [ ("args", List(List.map String args)) ]
        try
            (List [Atom "load"; String(args.[0])]) |> eval env |> string |> printStrLine
        with
        | LispException(error) -> string error |> printStrLine
    