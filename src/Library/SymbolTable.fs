namespace Library

module SymbolTable = 
    open System.Collections.Generic

    open LispVal
    open Errors

    let rec unpackNum = function
        | Number n -> n
        | String n -> 
            let success, result = System.Int32.TryParse n
            if success then
                result
            else
                throw (TypeMismatch ("number", String n))
        | List [n] -> unpackNum n
        | notNum -> throw (TypeMismatch ("number", notNum))
    
    let rec unpackBool = function
        | Bool b -> b
        | List [b]  -> unpackBool b
        | notBool -> throw (TypeMismatch ("bool", notBool))

    let rec unpackString = function
        | String s -> s
        | Number s -> string s
        | Bool s -> string s
        | List [s]  -> unpackString s
        | notString -> throw (TypeMismatch ("string", notString))

    let tryUnpacker unpack op arg1 arg2 = 
        try op (unpack arg1) (unpack arg2) 
        with _ -> false

    let numUnpackEq = tryUnpacker unpackNum (=)
    let boolUnpackEq = tryUnpacker unpackBool (=)
    let stringUnpackEq = tryUnpacker unpackString (=)

    let numericBinop op = function
        | [] -> throw (NumArgs(2, []))
        | [value] -> throw (NumArgs (2, [value]))
        | parms -> parms |> List.map unpackNum |> List.reduce op |> Number

    let boolBinop unpacker op = function
        | [left; right] -> Bool (op (unpacker left) (unpacker right))
        | parms -> throw (NumArgs (2, parms))

    let numBoolBinop = boolBinop unpackNum
    let boolBoolBinop = boolBinop unpackBool
    let stringBoolBinop = boolBinop unpackString

    let car = function
        | [List (x :: _)] -> x
        | [DottedList (x :: _, _ )] -> x
        | [badArg] -> throw (TypeMismatch ("pair", badArg))
        | badArgList -> throw (NumArgs (1, badArgList))
    
    let cdr = function
        | [List (_ :: xs)] -> List xs
        | [DottedList ([_], x)] -> x
        | [DottedList (_ :: xs, x)] -> DottedList (xs, x)
        | [badArg] -> throw (TypeMismatch ("pair", badArg))
        | badArgList -> throw (NumArgs (1, badArgList))

    let cons = function
        | [x1; List xs] -> List (x1 :: xs)
        | [x; DottedList (xs, last)] -> DottedList (x :: xs, last)
        | [x1; x2] -> DottedList ([x1], x2)
        | badArgList -> throw (NumArgs (2, badArgList))

    let rec eqvHelper arg1 arg2 =
        match (arg1, arg2) with
        | (Bool arg1, Bool arg2) -> arg1 = arg2
        | (Number arg1, Number arg2) -> arg1 = arg2
        | (String arg1, String arg2) -> arg1 = arg2
        | (Atom arg1, Atom arg2) -> arg1 = arg2
        | (DottedList (xs, x), DottedList (ys, y)) -> eqvHelper (List (xs @ [x])) (List (ys @ [y]))
        | (List l1, List l2) -> l1.Length = l2.Length && (List.forall2 eqvHelper l1 l2)
        | _ -> false

    let eqv = function
        | [arg1; arg2] -> Bool (eqvHelper arg1 arg2)
        | badArgList -> throw (NumArgs (2, badArgList))

    let equal = function
        | [arg1; arg2] -> Bool (eqvHelper arg1 arg2 || numUnpackEq arg1 arg2 || stringUnpackEq arg1 arg2 || boolUnpackEq arg1 arg2)
        | badArgList -> throw (NumArgs (2, badArgList))

    let rec primitives =
        [
            "+", numericBinop (+)
            "-", numericBinop (-)
            "*", numericBinop (*)
            "/", numericBinop (/)
            "mod", numericBinop (%)
            "=", numBoolBinop (=)
            "<", numBoolBinop (<)
            ">", numBoolBinop (>)
            "/=", numBoolBinop (<>)
            ">=", numBoolBinop (>=)
            "<=", numBoolBinop (<=)
            ">=", numBoolBinop (>=)
            ">=", numBoolBinop (>=)
            "&&", boolBoolBinop (&&)
            "||", boolBoolBinop (||)
            "string=?", stringBoolBinop (=)
            "string>?", stringBoolBinop (>)
            "string<?", stringBoolBinop (<)
            "string<=?", stringBoolBinop (<=)
            "string>=?", stringBoolBinop (>=)
            "car", car
            "cdr", cdr
            "cons", cons
            "eq?", eqv
            "eqv?", eqv
            "equal?", equal
        ]

    let nullEnv (): Env = ref List.empty

    let keyEq name (k, _) = name = k

    let isBound (env: Env) var  = !env |> List.exists (keyEq var)

    let getVar (env: Env) var  =
        let result = !env |> List.tryFind (keyEq var)
        match result with
        | None -> throw (UnboundVar("Getting an unbounded variable" , var))
        | Some(_, r) -> !r

    let setVar (env:Env) var value  =
        let result = !env |> List.tryFind (keyEq var)
        match result with
        | Some(_, v) -> 
            v := value
            value
        | None -> throw (UnboundVar("Setting an unbounded variable" , var))

    let defineVar (env:Env) var value =
        let result = !env |> List.tryFind (keyEq var)
        match result with
        | Some(_, v) -> 
            v := value
            value
        | None ->
            env := [var, ref value] @ !env
            value

    let bindVars bindings (env: Env)  =
        ref ((bindings |> List.map (fun (n, v) -> n , ref v)) @ !env)