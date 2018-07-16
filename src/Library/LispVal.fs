namespace Library

module LispVal = 
    open System.Collections.Generic

    type Env =  Dictionary<string, LispVal> 

    and FuncRecord = {
        parameters: string list;
        varargs: string option;
        body: LispVal list;
        closure: Env;
    }

    and LispVal = 
        | Atom of string
        | List of LispVal list
        | DottedList of LispVal list * LispVal
        | Number of int
        | String of string
        | Bool of bool
        | PrimitiveFunc of (LispVal list -> LispVal)
        | Func of FuncRecord
        | Nil

    let unwordsList = List.map string >> String.concat " "

    type LispVal with
        override this.ToString() =
            match this with
            | Atom atom -> atom
            | String str -> "\"" + str + "\""
            | Number num -> string num
            | Bool true -> "#t"
            | Bool false -> "#f"
            | Nil -> "Nil"
            | List contents -> "(" + unwordsList contents + ")"
            | DottedList (head, tail) -> "(" + unwordsList head + "." + string tail + ")"
            | PrimitiveFunc (_) -> "<primitive>"
            | Func (func) -> "(lambda (" +
                             unwordsList (func.parameters |> List.map String) +
                             (match func.varargs with
                                | None -> ""
                                | Some(arg) -> "." + arg) +
                             ") ...)"
