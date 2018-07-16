namespace Library

open LispVal
open Errors
open System.Collections.Generic

module SymbolTable = 

    let nullEnv (): Env = new Dictionary<_, _>()

    let isBound (env: Env) var = env.ContainsKey var

    let getVar (env: Env) var =
        try
            env.[var]
        with
            | :? KeyNotFoundException -> throw (UnboundVar("Getting an unbounded varialbe", var))

    let setVar (env: Env) var value =
        if isBound env var  then
            env.Add(var, value)
            value
        else
            throw (UnboundVar("Getting an unbounded varialbe", var))

    let defineVar  (env: Env) var value =
        env.Add(var, value)
        value

    let bindVar (env: Env) bindings  =
        for binding in bindings do env.Add binding