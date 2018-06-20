namespace Library

module Eval =
    open System
    open LispVal
    open Errors

    let rec unpackNum = function
        | Number(n) -> n
        | String(n) -> 
            let success, result = System.Int32.TryParse n
            if success then
                result
            else
                throw (TypeMismatch ("number", String n))
        | List [n] -> unpackNum n
        | notNum -> throw (TypeMismatch ("number", notNum))

    let numericBinop op = function
        | [] -> throw (NumArgs(2, []))
        | value::[] -> throw (NumArgs (2, [value]))
        | parms -> parms |> List.map unpackNum |> List.reduce op |> Number

    let rec primitives =
        dict [
            "+", numericBinop (+)
            "-", numericBinop (-)
            "*", numericBinop (*)
            "/", numericBinop (/)
            "mod", numericBinop(%)
        ]

    let rec Eval = function
        | String _ as v -> v
        | Number _ as v -> v
        | Bool _ as v -> v
        // | Atom _ as v -> v
        | List[Atom "quote"; v] -> v
        | List (Atom func :: args) ->
            args |> List.map Eval |> apply func
        | badForm -> throw (BadSpecialForm ("Unrecognized special form", badForm))

    and apply func args =
        if primitives.ContainsKey(func) |> not then
            throw (NotFunction ("Unrecognized primitive function args", func))
        else
            primitives.[func] args
