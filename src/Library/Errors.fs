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

    let ShowError = function
        | UnboundVar (message, varName) -> message + ": " + varName
        | BadSpecialForm (message, form) -> message + ": " + ShowVal form
        | NotFunction (message, func) -> message + ": " + func
        | NumArgs (expected, found) -> "Expected: " + string expected + " args; found values " + UnwordsList found
        | TypeMismatch (expected, found) -> "Invalid type: expected " + expected + ", found " + ShowVal found
        | Parser (message, parseErr) -> "Parse error at " + message
        | IOError message -> "IO Error: " + message
        | Default message -> "Error: " + message
    
    exception LispException of LispError

    let throw le = raise (LispException(le))