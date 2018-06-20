namespace Library

module Eval =
    open LispVal

    let rec unpackNum = function
        | Number(n) -> n
        | String(n) -> 
            let success, result = System.Int32.TryParse n
            if success then result else 0
        | List [n] -> unpackNum n
        | _ -> 0

    let numericBinop op parms =
        parms |> List.map unpackNum |> List.reduce op |> Number

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
        | Atom _ as v -> v
        | List[Atom "quote"; v] -> v
        | List (Atom func :: args) ->
            args |> List.map Eval |> apply func
        | _ -> Atom ("lispval")

    and apply func args =
        if primitives.ContainsKey(func) |> not then
            Bool false
        else
            primitives.[func] args
