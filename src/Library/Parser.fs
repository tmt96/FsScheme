namespace Library

open System
open System
module Parser =

    open FParsec.Primitives 
    open FParsec.CharParsers
    open FParsec
    open LispVal

    type LispParser<'a> = Parser<'a, unit>
    type LispParser = Parser<LispVal, unit>

    let symbol: LispParser<char> = anyOf "!$%&|*+-/:<=>?@^_~#"

    let chr c = skipChar c

    let endBy p sep = many (p .>> sep)

    let parseString: LispParser = chr '"' >>. manyChars (noneOf "\"\\") |>> String .>> chr '"'

    let parseAtom: LispParser = parse {
        let! first = letter <|> symbol
        let! rest = manyChars (letter <|> digit <|> symbol)
        return match string first + rest with
                | "#t" -> Bool(true)
                | "#f" -> Bool(false)
                | atom -> Atom(atom)
    }

    let parseNumber: LispParser = 
        many1Chars digit |>> (System.Int32.Parse >> Number)


    let parseExpr, parseExprRef : LispParser * LispParser ref = createParserForwardedToRef()

    let parseNormList: LispParser =
        sepBy parseExpr spaces1 |>> List

    let parseDottedList: LispParser = parse {
        let! head = endBy parseExpr spaces1
        let! tail = chr '.' >>. spaces1 >>. parseExpr
        return DottedList(head, tail)
    }

    let parseList: LispParser = parse { 
       do! chr '(' 
       let! x = (attempt parseNormList) <|> parseDottedList 
       do! chr ')' 
       return x 
    }

    let parseQuoted: LispParser = 
        chr '\'' >>. parseExpr |>> fun expr -> List [Atom "quote"; expr]

    do parseExprRef :=
        parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> parseList

    let ReadExpr input =
        match runParserOnString parseExpr () "scheme" input with
        | Success (result, _, _) -> 
            result
        | Failure (errString, _, _) -> String ("No match: " + errString)
