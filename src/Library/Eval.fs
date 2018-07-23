namespace Library

open System.Collections.Generic
module Eval =
    open System.Linq

    open LispVal
    open Errors
    open SymbolTable

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
    
    let unpackBool = function
        | Bool b -> b
        | notBool -> throw (TypeMismatch ("bool", notBool))

    let unpackString = function
        | String s -> s
        | Number s -> string s
        | Bool s -> string s
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
        | List (Atom "define" :: List (Atom var :: parms) :: body) -> makeNormalFunc env parms body |> defineVar env var
        | List (Atom "define" :: DottedList ((Atom var :: parms), varargs) :: body) -> makeVarArgs varargs env parms body |> defineVar env var
        | List (Atom "lambda" :: List parms :: body) -> makeNormalFunc env parms body
        | List (Atom "lambda" :: DottedList (parms, varargs) :: body) -> makeVarArgs varargs env parms body
        | List (Atom "lambda" :: ((Atom _) as varargs) :: body) -> makeVarArgs varargs env [] body
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

        | funcName -> throw (NotFunction("Expecting a function", string funcName))                
