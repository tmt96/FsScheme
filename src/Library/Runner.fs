namespace Library

module Runner = 
    open LispVal
    open System

    let defaultPrompt = "scheme>>"
    
    let printStr (s: string) = Console.WriteLine(s)

    let ReadPrompt = 
        printStr defaultPrompt
        Console.ReadLine

    let RunFile filename args = ()

    let RunRepl () = 
        ReadPrompt |> Console.WriteLine

