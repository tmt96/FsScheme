namespace Library

module Eval =
    open System
    open System.IO
    open LispVal
    open Errors
    open SymbolTable
    open Parser

    let makeFunc varargs env parms body =
        Func
            { parameters = parms |> List.map string
              varargs = varargs
              body = body
              closure = env }

    let makeNormalFunc = makeFunc None

    let makeVarArgs =
        string
        >> Some
        >> makeFunc

    let fileIOFunction func =
        function
        | [ String fileName ] -> func (fileName)
        | [] -> throw (IOError("No filename provided"))
        | args -> throw (NumArgs(1, args))

    let load = fileIOFunction (File.ReadAllText >> readExprList)

    let makePort fileAccessMode =
        fileIOFunction (fun fileName -> File.Open(fileName, FileMode.OpenOrCreate, fileAccessMode) :> Stream |> Port)

    let closePort =
        function
        | [ Port port ] ->
            port.Close()
            Bool true
        | _ -> Bool false

    let rec readProc =
        function
        | [ Port port ] ->
            use reader = new StreamReader(port)
            reader.ReadLine() |> readExpr
        | [] -> readProc [ Port(Console.OpenStandardInput()) ]
        | args -> throw (NumArgs(1, args))

    let rec writeProc =
        function
        | [ obj; Port port ] ->
            use writer = new StreamWriter(port)
            writer.WriteLine(string obj)
            Bool true
        | [ obj ] ->
            writeProc
                [ obj
                  Port(Console.OpenStandardOutput()) ]
        | args -> throw (NumArgs(1, args))

    let readContents = fileIOFunction (File.ReadAllText >> String)

    let readAll fileName = load fileName |> List

    let rec ioPrimitives =
        [ "apply", applyProc
          "open-input-file", makePort FileAccess.Read
          "open-output-file", makePort FileAccess.Write
          "close-input-port", closePort
          "close-output-port", closePort
          "read", readProc
          "write", writeProc
          "read-contents", readContents
          "read-all", readAll ]

    and eval env =
        function
        | String _ as v -> v
        | Number _ as v -> v
        | Bool _ as v -> v
        | Atom v -> getVar env v
        | List [ Atom "quote"; v ] -> v
        | List [ Atom "if"; pred; conseq; alt ] ->
            match eval env pred with
            | Bool false -> eval env alt
            | _ -> eval env conseq
        | List [ Atom "define"; Atom var; form ] -> eval env form |> defineVar env var
        | List(Atom "define" :: List(Atom var :: parms) :: body) -> makeNormalFunc env parms body |> defineVar env var
        | List(Atom "define" :: DottedList((Atom var :: parms), varargs) :: body) ->
            makeVarArgs varargs env parms body |> defineVar env var
        | List(Atom "lambda" :: (List parms :: body)) -> makeNormalFunc env parms body
        | List(Atom "lambda" :: DottedList(parms, varargs) :: body) -> makeVarArgs varargs env parms body
        | List(Atom "lambda" :: ((Atom _) as varargs) :: body) -> makeVarArgs varargs env [] body
        | List [ Atom "set!"; Atom var; form ] -> eval env form |> setVar env var
        | List [ Atom "load"; filename ] ->
            load [ filename ]
            |> List.map (eval env)
            |> List.last
        | List(func :: args) ->
            let f = eval env func
            let argVals = List.map (eval env) args
            apply f argVals
        | badForm -> throw (BadSpecialForm("Unrecognized special form", badForm))

    and apply func args =
        match func with
        | PrimitiveFunc func -> func args
        | Func { parameters = parms; varargs = varargs; body = body; closure = closure } ->
            string func |> printfn "%s"
            printfn "parms: %A, args: %A, body: %A" parms args body
            if parms.Length <> args.Length && varargs.IsNone then
                throw (NumArgs(parms.Length, args))
            else
                let remainingArgs = args |> List.skip parms.Length

                let evalBody env =
                    body
                    |> List.map (eval env)
                    |> List.last

                let bindVarArgs arg env =
                    match arg with
                    | Some(argName) -> bindVars env [ argName, (List remainingArgs) ]
                    | None -> env
                bindVars closure (Seq.zip parms args |> Seq.toList)
                |> bindVarArgs varargs
                |> evalBody
        | IOFunc func -> func args
        | funcName -> throw (NotFunction("Expecting a function", string funcName))

    and applyProc =
        function
        | [ func; List args ] -> apply func args
        | func :: args -> apply func args
        | [] -> throw (Default("Expecting a function, got an empty list"))
