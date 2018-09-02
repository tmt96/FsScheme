namespace Library

module Eval =
    open System
    open System.IO
    open LispVal
    open Errors
    open SymbolTable
    open Parser

    let makeFunc varargs env parms body =
        Func {
            parameters = parms |> List.map string;
            varargs = varargs;
            body = body;
            closure = env;
        }

    let makeNormalFunc = makeFunc None
    
    let makeVarArgs = string >> Some >> makeFunc

    let rec eval env = function
        | String _ as v -> v
        | Number _ as v -> v
        | Bool _ as v -> v
        | Atom v -> getVar env v
        | List [Atom "quote"; v] -> v
        | List [Atom "if"; pred; conseq; alt] -> 
            match eval env pred with
            | Bool false -> eval env alt
            | _ -> eval env conseq
        | List [Atom "define"; Atom var; form] ->
            eval env form |> defineVar env var
        | List (Atom "define" :: List(Atom var :: parms) :: body) -> 
            makeNormalFunc env parms body |> defineVar env var
        | List (Atom "define" :: DottedList((Atom var :: parms), varargs) :: body) ->
            makeVarArgs varargs env parms body |> defineVar env var
        | List (Atom "lambda" :: List parms :: body) ->
            makeNormalFunc env parms body
        | List (Atom "lambda" :: DottedList (parms, varargs) :: body) ->    
            makeVarArgs varargs env parms body
        | List (Atom "lambda" :: ((Atom _) as varargs) :: body) ->
            makeVarArgs varargs env [] body
        | List [Atom "set!"; Atom var; form] ->
            eval env form |> setVar env var 
        | List (func :: args) ->
            let f = eval env func
            let argVals = args |> List.map (eval env)
            apply f argVals
        | badForm -> throw (BadSpecialForm ("Unrecognized special form", badForm))

    and apply func args = 
        match func with
        | PrimitiveFunc func -> func args
        | Func {parameters = parms; varargs = varargs; body = body; closure = closure} ->
            if parms.Length <> args.Length && varargs.IsNone then
                throw (NumArgs (parms.Length, args))
            else
                let remainingArgs = args |> List.skip parms.Length
                let evalBody env = body |> List.map (eval env) |> List.last
                let bindVarArgs arg env = 
                    match arg with
                    | Some(argName) -> bindVars env [argName, (List remainingArgs)]
                    | None -> env

                bindVars closure (List.zip parms args)
                |> bindVarArgs varargs
                |> evalBody
        | IOFunc func -> func args
        | funcName -> throw (NotFunction("Expecting a function", string funcName))                

    let applyProc = function
        | [func; List args] -> apply func args
        | func::args -> apply func args
        | [] -> throw (Default("Expecting a function, got an empty list"))

    let fileIOFunction func = function
        | [String fileName] -> func(fileName)
        | [] -> throw(IOError("No filename provided"))
        | args -> throw(NumArgs(1, args))

    let makePort fileAccessMode =
        fileIOFunction(fun fileName -> 
            File.Open(fileName, FileMode.OpenOrCreate, fileAccessMode) :> Stream |> Port
        )

    let closePort = function
        | [Port port] -> port.Close(); Bool true
        | _ -> Bool false

    let rec readProc = function
        | [Port port] -> 
            use reader = new StreamReader(port)
            reader.ReadLine() |> readExpr
        | [] -> readProc [Port (Console.OpenStandardInput())]
        | args -> throw(NumArgs(1, args))

    let rec writeProc = function
        | [obj; Port port] ->
            use writer = new StreamWriter(port)
            writer.WriteLine(string obj)
            Bool true
        | [obj] -> 
            writeProc [obj; Port (Console.OpenStandardOutput())]
        | args -> throw (NumArgs(1, args)) 

    let readContents = 
        fileIOFunction(fun fileName -> String (File.ReadAllText(fileName)))

    let load =
        fileIOFunction(fun fileName -> File.ReadAllText(fileName) |> readExprList)

    let readAll fileName = load fileName |> List
