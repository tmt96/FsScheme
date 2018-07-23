namespace Library

open System.Linq
module SymbolTable = 
    open LispVal
    open Errors
    open System.Collections.Generic
    open System.Linq


    let nullEnv (): Env = new Dictionary<_, _>()

    let isBound (env: Env) var = env.ContainsKey var

    let getVar (env: Env) var =
        try
            env.[var]
        with
            | :? KeyNotFoundException -> throw (UnboundVar("Getting an unbounded varialbe", var))

    let setVar (env: Env) var value =
        if isBound env var  then
            env.[var] <- value
            value
        else
            throw (UnboundVar("Getting an unbounded varialbe", var))

    let defineVar  (env: Env) var value =
        env.[var] <- value
        value

    let bindVars (env: Env) bindings  =
        for key, value in bindings do 
            env.[key] <- value
        env