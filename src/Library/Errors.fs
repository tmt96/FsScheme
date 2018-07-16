namespace Library

module Errors =
    open LispVal
    open FParsec.Error

    type LispError =
        | NumArgs of int * LispVal list
        | TypeMismatch of string * LispVal
        | Parser of string * ParserError
        | BadSpecialForm of string * LispVal
        | NotFunction of string * string
        | UnboundVar of string * string
        | IOError of string
        | Default of string

        override this.ToString() =
            match this with
            | UnboundVar (message, varName) -> message + ": " + varName
            | BadSpecialForm (message, form) -> message + ": " + string form
            | NotFunction (message, func) -> message + ": " + func
            | NumArgs (expected, found) -> "Expected: " + string expected + " args; found values " + unwordsList found
            | TypeMismatch (expected, found) -> "Invalid type: expected " + expected + ", found " + string found
            | Parser (message, _) -> "Parse error at " + message
            | IOError message -> "IO Error: " + message
            | Default message -> "Error: " + message
    
    exception LispException of LispError

    let throw le = raise (LispException(le))