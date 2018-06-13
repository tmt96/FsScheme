namespace Library

module LispVal = 

    type Env =  Map<string, LispVal ref> 

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

    let rec ShowVal = function
        | Atom atom -> atom
        | String str -> "\"" + str + "\""
        | Number num -> string num
        | Bool true -> "#t"
        | Bool false -> "#f"
        | Nil -> "Nil"
        | List contents -> "(" + UnwordsList contents + ")"
        | DottedList (head, tail) -> "(" + UnwordsList head + "." + ShowVal tail + ")"
        | PrimitiveFunc (_) -> "<primitive>"
        | Func (func) -> "(lambda (" +
                         UnwordsList (func.parameters |> List.map String) +
                         (match func.varargs with
                            | None -> ""
                            | Some(arg) -> "." + arg) +
                         ") ...)"

    and UnwordsList = List.map ShowVal >> String.concat " "