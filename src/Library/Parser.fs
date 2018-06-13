namespace Library

module Parser =

    open FParsec.Primitives 
    open FParsec.CharParsers 
    open FParsec
    open LispVal

    type LispParser<'a> = Parser<'a, unit>
    type LispParser = Parser<LispVal, unit>

    let symbol: LispParser<char> = anyOf "!$%&|*+-/:<=>?@^_~#"

    let spaces1: LispParser<unit> = skipMany1 spaces

    let chr c = skipChar c

    let parseString: LispParser = chr '"' >>. manyChars (noneOf "\"\\") |>> String .>> chr '"'

    let parseAtom: LispParser = parse {
        let! first = letter <|> symbol
        let! rest = many (letter <|> digit <|> symbol)
        return match string (first::rest) with
                | "#t" -> Bool(true)
                | "#f" -> Bool(false)
                | atom -> Atom(atom)
    }

    let parseNumber: LispParser = 
        many1Chars digit |>> (System.Int32.Parse >> Number)


    let parseExpr, parseExprRef : LispParser * LispParser ref = createParserForwardedToRef()

    let parseNormList: LispParser =
        sepBy parseExpr spaces |>> List

    let parseDottedList: LispParser = parse {
        let! head = sepEndBy parseExpr spaces
        let! tail = chr '.' >>. spaces >>. parseExpr
        return DottedList(head, tail)
    }

    let parseList: LispParser =
        chr '(' >>. (attempt parseNormList <|> parseDottedList) .>> chr '('

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
        | Success (result, _, _) -> "Found value " + string result
        | Failure (errString, _, _) -> "No match: " + errString