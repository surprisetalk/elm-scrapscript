module Scrapscript exposing (..)

-- TODO: only exposing (Scrap, run, toString)

import Char
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Step(..), end, lazy, oneOf, succeed, symbol)
import Parser.Extras as Parser
import Result exposing (Result)
import Result.Extra as Result
import Set exposing (Set)


type Token
    = StringLit String
    | IntLit Int
    | FloatLit Float
    | VariantToken String
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket
    | Operator String
    | Name String
    | BytesLit ( String, Int )


type alias LexerState =
    { text : String
    , idx : Int
    , lineno : Int
    , colno : Int
    , line : String
    }


isIdentifierStartChar : Char -> Bool
isIdentifierStartChar c =
    Char.isAlpha c || c == '$'


isIdentifierChar : Char -> Bool
isIdentifierChar c =
    Char.isAlphaNum c || c == '-' || c == '$' || c == '\''


isOperatorChar : Char -> Bool
isOperatorChar c =
    Set.member c (Set.fromList [ '+', '-', '*', '/', '<', '>', '=', '!', '&', '|', '^', '%', '~' ])


isSpaceChar : Char -> Bool
isSpaceChar c =
    c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t'


lex : String -> Result String (List Token)
lex =
    Parser.run
        (Parser.succeed List.concat
            |= Parser.many exprParser
            |. Parser.end
        )
        >> Result.mapError Debug.toString


exprParser : Parser (List Token)
exprParser =
    Parser.succeed (::)
        |= tokenParser
        |= Parser.many
            (Parser.succeed identity
                |. Parser.chompIf isSpaceChar
                |. Parser.chompWhile isSpaceChar
                |= tokenParser
            )


tokenParser : Parser Token
tokenParser =
    oneOf
        [ stringLitParser
        , numberParser
        , variantParser
        , bytesParser
        , symbolParser
        , operatorParser
        , nameParser
        ]


stringLitParser : Parser Token
stringLitParser =
    Parser.succeed StringLit
        |. symbol "\""
        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"'))
        |. symbol "\""


numberParser : Parser Token
numberParser =
    Parser.number
        { int = Just IntLit
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just FloatLit
        }


variantParser : Parser Token
variantParser =
    Parser.succeed VariantToken
        |. symbol "#"
        |= varParser


bytesParser : Parser Token
bytesParser =
    Parser.succeed identity
        |. symbol "~~"
        |= Parser.getChompedString (Parser.chompWhile (\c -> not (isSpaceChar c)))
        |> Parser.andThen
            (\str ->
                case String.split "'" str of
                    [] ->
                        Parser.problem "Invalid bytes literal"

                    [ v ] ->
                        Parser.succeed (BytesLit ( v, 64 ))

                    b :: v :: _ ->
                        case String.toInt b of
                            Just base ->
                                Parser.succeed (BytesLit ( v, base ))

                            Nothing ->
                                Parser.problem "Invalid base for bytes literal"
            )


symbolParser : Parser Token
symbolParser =
    oneOf
        [ Parser.map (\_ -> LeftParen) (symbol "(")
        , Parser.map (\_ -> RightParen) (symbol ")")
        , Parser.map (\_ -> LeftBrace) (symbol "{")
        , Parser.map (\_ -> RightBrace) (symbol "}")
        , Parser.map (\_ -> LeftBracket) (symbol "[")
        , Parser.map (\_ -> RightBracket) (symbol "]")
        ]


operatorParser : Parser Token
operatorParser =
    Parser.getChompedString (Parser.chompWhile isOperatorChar)
        |> Parser.andThen
            (\op ->
                if String.isEmpty op then
                    Parser.problem "Expected operator"

                else
                    Parser.succeed (Operator op)
            )


varParser : Parser String
varParser =
    Parser.variable
        { start = isIdentifierStartChar
        , inner = isIdentifierChar
        , reserved = Set.fromList []
        }


nameParser : Parser Token
nameParser =
    varParser |> Parser.map Name


type Scrap
    = Int Int
    | Float Float
    | String String
    | Bytes String
    | Var String
    | Binop BinopKind ( Scrap, Scrap )
    | List (List Scrap)
    | Record (List ( String, Scrap ))
    | Spread (Maybe String)
    | Function ( Scrap, Scrap )
    | Apply ( Scrap, Scrap )
    | Where ( Scrap, Scrap )
    | Assert ( Scrap, Scrap )
    | Assign ( Scrap, Scrap )
    | MatchFunction (List MatchCase)
    | Variant ( String, Scrap )
    | Access ( Scrap, Scrap )
    | Hole
    | Closure ( Env, Scrap )
    | EnvObject Env
    | NativeFunction (Scrap -> Result String Scrap)


type BinopKind
    = ADD
    | SUB
    | MUL
    | DIV
    | FLOOR_DIV
    | EXP
    | MOD
    | EQUAL
    | NOT_EQUAL
    | LESS
    | GREATER
    | LESS_EQUAL
    | GREATER_EQUAL
    | BOOL_AND
    | BOOL_OR
    | STRING_CONCAT
    | LIST_CONS
    | LIST_APPEND
    | HASTYPE
    | RIGHT_EVAL


type MatchCase
    = MatchCase ( Scrap, Scrap )


true : Scrap
true =
    Variant ( "true", Hole )


false : Scrap
false =
    Variant ( "false", Hole )


type alias Prec =
    { pl : Float
    , pr : Float
    }


type alias ParserState =
    { tokens : List Token
    , counter : Int
    }


lp : Float -> Prec
lp x =
    { pl = x, pr = x + 0.1 }


rp : Float -> Prec
rp x =
    { pl = x + 0.1, pr = x }


np : Float -> Prec
np x =
    { pl = x, pr = x }


xp : Float -> Prec
xp x =
    { pl = x, pr = x }


ps : Dict String Prec
ps =
    Dict.fromList
        [ ( "::", { pl = 2000, pr = 2000 } )
        , ( "@", rp 1001 )
        , ( "", rp 1000 )
        , ( ">>", lp 14 )
        , ( "<<", lp 14 )
        , ( "^", rp 13 )
        , ( "*", rp 12 )
        , ( "/", rp 12 )
        , ( "//", lp 12 )
        , ( "%", lp 12 )
        , ( "+", lp 11 )
        , ( "-", lp 11 )
        , ( ">*", rp 10 )
        , ( "++", rp 10 )
        , ( ">+", lp 10 )
        , ( "+<", rp 10 )
        , ( "==", np 9 )
        , ( "/=", np 9 )
        , ( "<", np 9 )
        , ( ">", np 9 )
        , ( "<=", np 9 )
        , ( ">=", np 9 )
        , ( "&&", rp 8 )
        , ( "||", rp 7 )
        , ( "|>", rp 6 )
        , ( "<|", lp 6 )
        , ( "#", lp 5.5 )
        , ( "->", lp 5 )
        , ( "|", rp 4.5 )
        , ( ":", lp 4.5 )
        , ( "=", rp 4 )
        , ( "!", lp 3 )
        , ( ".", rp 3 )
        , ( "?", rp 3 )
        , ( ",", xp 1 )
        , ( "...", xp 0 ) -- TODO: Fix precedence for spread
        ]


highestPrec : Float
highestPrec =
    ps
        |> Dict.values
        |> List.map (\p -> max p.pl p.pr)
        |> List.maximum
        |> Maybe.withDefault 0


gensym : ParserState -> ( String, ParserState )
gensym state =
    let
        newCounter =
            state.counter + 1
    in
    ( "$v" ++ String.fromInt newCounter
    , { state | counter = newCounter }
    )


parse : List Token -> Result String Scrap
parse tokens =
    case parseHelper { tokens = tokens, counter = -1 } 0 of
        Ok ( obj, _ ) ->
            Ok obj

        Err err ->
            Err err


parseHelper : ParserState -> Float -> Result String ( Scrap, ParserState )
parseHelper state p =
    Result.andThen (parseInfixAndApplication state p) <|
        case state.tokens of
            [] ->
                Err "Unexpected EOF"

            token :: restTokens ->
                let
                    newState =
                        { state | tokens = restTokens }
                in
                case token of
                    IntLit value ->
                        Ok ( Int value, newState )

                    FloatLit value ->
                        Ok ( Float value, newState )

                    Name value ->
                        Ok ( Var value, newState )

                    VariantToken value ->
                        parseHelper newState (Dict.get "" ps |> Maybe.map .pr |> Maybe.withDefault 0 |> (+) 1)
                            |> Result.map (\( obj, s ) -> ( Variant ( value, obj ), s ))

                    BytesLit ( value, base ) ->
                        case base of
                            85 ->
                                Ok ( Bytes value, newState )

                            64 ->
                                Ok ( Bytes value, newState )

                            32 ->
                                Ok ( Bytes value, newState )

                            16 ->
                                Ok ( Bytes value, newState )

                            _ ->
                                Err ("Unexpected base " ++ String.fromInt base)

                    StringLit value ->
                        Ok ( String value, newState )

                    Operator "..." ->
                        case newState.tokens of
                            (Name name) :: rest ->
                                Ok ( Spread (Just name), { newState | tokens = rest } )

                            _ ->
                                Ok ( Spread Nothing, newState )

                    Operator "|" ->
                        parseMatchFunction newState

                    LeftParen ->
                        case newState.tokens of
                            RightParen :: rest ->
                                Ok ( Hole, { newState | tokens = rest } )

                            _ ->
                                parseHelper newState 0
                                    |> Result.andThen
                                        (\( obj, s ) ->
                                            case s.tokens of
                                                RightParen :: rest ->
                                                    Ok ( obj, { s | tokens = rest } )

                                                _ ->
                                                    Err "TODO: unexpected token"
                                         -- (UnexpectedToken (Maybe.withDefault LeftParen (List.head s.tokens)))
                                        )

                    LeftBracket ->
                        parseList newState

                    LeftBrace ->
                        parseRecord newState

                    Operator "-" ->
                        parseHelper newState (highestPrec + 1)
                            |> Result.map (\( obj, s ) -> ( Binop SUB ( Int 0, obj ), s ))

                    _ ->
                        Err "TODO: unexpected token"



-- (UnexpectedToken token)


parseRecord : ParserState -> Result String ( Scrap, ParserState )
parseRecord state =
    let
        parseRecordHelper : ParserState -> List ( String, Scrap ) -> Result String ( Scrap, ParserState )
        parseRecordHelper s acc =
            case s.tokens of
                RightBrace :: rest ->
                    Ok ( Record acc, { s | tokens = rest } )

                _ ->
                    parseAssign s
                        |> Result.andThen
                            (\s_ ->
                                case s_ of
                                    ( Assign ( Var name, value ), newState ) ->
                                        case newState.tokens of
                                            RightBrace :: rest ->
                                                Ok ( Record (( name, value ) :: acc), { newState | tokens = rest } )

                                            (Operator ",") :: rest ->
                                                parseRecordHelper { newState | tokens = rest } (( name, value ) :: acc)

                                            _ ->
                                                Err "TODO: unexpected token"

                                    -- (UnexpectedToken (Maybe.withDefault LeftBrace (List.head newState.tokens)))
                                    _ ->
                                        Err "TODO: unexpected record deconstruction"
                            )
    in
    parseRecordHelper state []


parseList : ParserState -> Result String ( Scrap, ParserState )
parseList state =
    let
        parseListHelper : ParserState -> List Scrap -> Result String ( Scrap, ParserState )
        parseListHelper s acc =
            case s.tokens of
                RightBracket :: rest ->
                    Ok ( List (List.reverse acc), { s | tokens = rest } )

                _ ->
                    parseHelper s 2
                        |> Result.andThen
                            (\( item, newState ) ->
                                case newState.tokens of
                                    RightBracket :: rest ->
                                        Ok ( List (List.reverse (item :: acc)), { newState | tokens = rest } )

                                    (Operator ",") :: rest ->
                                        parseListHelper { newState | tokens = rest } (item :: acc)

                                    _ ->
                                        Err "TODO: unexpected token"
                             -- (UnexpectedToken (Maybe.withDefault LeftBracket (List.head newState.tokens)))
                            )
    in
    parseListHelper state []


parseAssign : ParserState -> Result String ( Scrap, ParserState )
parseAssign state =
    parseHelper state 0
        |> Result.andThen
            (\( obj, newState ) ->
                case obj of
                    Spread _ ->
                        Ok ( Assign ( Var "...", obj ), newState )

                    _ ->
                        case newState.tokens of
                            (Operator "=") :: rest ->
                                parseHelper { newState | tokens = rest } (getPrec "=").pl
                                    |> Result.map (\( value, s ) -> ( Assign ( obj, value ), s ))

                            _ ->
                                Err "Failed to parse variable assignment in record constructor"
            )


parseMatchFunction : ParserState -> Result String ( Scrap, ParserState )
parseMatchFunction state =
    let
        parseCases : ParserState -> List MatchCase -> Result String ( Scrap, ParserState )
        parseCases s acc =
            parseHelper s (getPrec "|").pl
                |> Result.andThen
                    (\( expr, newState ) ->
                        case expr of
                            Function ( arg, body ) ->
                                let
                                    newCase =
                                        MatchCase ( arg, body )
                                in
                                case newState.tokens of
                                    (Operator "|") :: rest ->
                                        parseCases { newState | tokens = rest } (newCase :: acc)

                                    _ ->
                                        Ok ( MatchFunction (List.reverse (newCase :: acc)), newState )

                            _ ->
                                Err "Expected function in match expression"
                    )
    in
    parseCases state []


getPrec : String -> Prec
getPrec op =
    Dict.get op ps |> Maybe.withDefault { pl = 0, pr = 0 }


parseInfixAndApplication : ParserState -> Float -> ( Scrap, ParserState ) -> Result String ( Scrap, ParserState )
parseInfixAndApplication originalState minPrec ( left, state ) =
    case state.tokens of
        [] ->
            Ok ( left, state )

        (Operator op) :: rest ->
            let
                prec =
                    getPrec op
            in
            if prec.pl < minPrec then
                Ok ( left, state )

            else
                parseHelper { state | tokens = rest } prec.pr
                    |> Result.andThen
                        (\( right, newState ) ->
                            case op of
                                "=" ->
                                    case left of
                                        Var _ ->
                                            parseInfixAndApplication originalState minPrec ( Assign ( left, right ), newState )

                                        _ ->
                                            Err "Expected variable in assignment"

                                "->" ->
                                    parseInfixAndApplication originalState minPrec ( Function ( left, right ), newState )

                                "|>" ->
                                    parseInfixAndApplication originalState minPrec ( Apply ( right, left ), newState )

                                "<|" ->
                                    parseInfixAndApplication originalState minPrec ( Apply ( left, right ), newState )

                                ">>" ->
                                    let
                                        ( varName, genState ) =
                                            gensym newState

                                        newFunc =
                                            Function ( Var varName, Apply ( right, Apply ( left, Var varName ) ) )
                                    in
                                    parseInfixAndApplication originalState minPrec ( newFunc, genState )

                                "<<" ->
                                    let
                                        ( varName, genState ) =
                                            gensym newState

                                        newFunc =
                                            Function ( Var varName, Apply ( left, Apply ( right, Var varName ) ) )
                                    in
                                    parseInfixAndApplication originalState minPrec ( newFunc, genState )

                                "." ->
                                    parseInfixAndApplication originalState minPrec ( Where ( left, right ), newState )

                                "?" ->
                                    parseInfixAndApplication originalState minPrec ( Assert ( left, right ), newState )

                                "@" ->
                                    parseInfixAndApplication originalState minPrec ( Access ( left, right ), newState )

                                _ ->
                                    parseInfixAndApplication originalState minPrec ( Binop (toBinopKind op) ( left, right ), newState )
                        )

        _ ->
            let
                prec =
                    getPrec ""
            in
            if prec.pl < minPrec then
                Ok ( left, state )

            else
                parseHelper state prec.pr
                    |> Result.andThen
                        (\( right, newState ) ->
                            parseInfixAndApplication originalState minPrec ( Apply ( left, right ), newState )
                        )


toBinopKind : String -> BinopKind
toBinopKind op =
    case op of
        "+" ->
            ADD

        "-" ->
            SUB

        "*" ->
            MUL

        "/" ->
            DIV

        "%" ->
            MOD

        "&&" ->
            BOOL_AND

        "||" ->
            BOOL_OR

        "==" ->
            EQUAL

        "/=" ->
            NOT_EQUAL

        "<" ->
            LESS

        ">" ->
            GREATER

        "<=" ->
            LESS_EQUAL

        ">=" ->
            GREATER_EQUAL

        _ ->
            Debug.todo ("Unhandled operator: " ++ op)


type alias Env =
    Dict String Scrap


match : ( Scrap, Scrap ) -> Result String (List ( String, Scrap ))
match ( obj, pattern ) =
    case pattern of
        Hole ->
            case obj of
                Hole ->
                    Ok []

                _ ->
                    Err "Match failed: expected Hole"

        Int patternValue ->
            case obj of
                Int objValue ->
                    if objValue == patternValue then
                        Ok []

                    else
                        Err "Match failed: Int values don't match"

                _ ->
                    Err "Match failed: expected Int"

        Float _ ->
            Err "Pattern matching is not supported for Floats"

        String patternValue ->
            case obj of
                String objValue ->
                    if objValue == patternValue then
                        Ok []

                    else
                        Err "Match failed: String values don't match"

                _ ->
                    Err "Match failed: expected String"

        Var name ->
            Ok [ ( name, obj ) ]

        Variant ( patternTag, patternValue ) ->
            case obj of
                Variant ( objTag, objValue ) ->
                    if objTag == patternTag then
                        match ( objValue, patternValue )

                    else
                        Err "Match failed: Variant tags don't match"

                _ ->
                    Err "Match failed: expected Variant"

        Record patternData ->
            case obj of
                Record objData ->
                    matchRecord objData patternData

                _ ->
                    Err "Match failed: expected Record"

        List patternItems ->
            case obj of
                List objItems ->
                    matchList objItems patternItems

                _ ->
                    Err "Match failed: expected List"

        _ ->
            Err ("Match not implemented for " ++ Debug.toString pattern)


matchRecord : List ( String, Scrap ) -> List ( String, Scrap ) -> Result String (List ( String, Scrap ))
matchRecord objData patternData =
    let
        matchHelper : List ( String, Scrap ) -> List ( String, Scrap ) -> Result String (List ( String, Scrap ))
        matchHelper remainingPattern accResult =
            case remainingPattern of
                [] ->
                    if List.length objData == List.length patternData then
                        Ok accResult

                    else
                        Err "Match failed: Record sizes don't match"

                ( key, patternItem ) :: rest ->
                    case Dict.get key (Dict.fromList objData) of
                        Just objItem ->
                            match ( objItem, patternItem )
                                |> Result.andThen
                                    (\partialResult ->
                                        matchHelper rest (accResult ++ partialResult)
                                    )

                        Nothing ->
                            Err ("Match failed: Key " ++ key ++ " not found in object")
    in
    matchHelper patternData []


matchList : List Scrap -> List Scrap -> Result String (List ( String, Scrap ))
matchList objItems patternItems =
    let
        matchHelper : List Scrap -> List Scrap -> List ( String, Scrap ) -> Result String (List ( String, Scrap ))
        matchHelper remainingObj remainingPattern accResult =
            case ( remainingObj, remainingPattern ) of
                ( _, [] ) ->
                    if List.isEmpty remainingObj then
                        Ok accResult

                    else
                        Err "Match failed: List sizes don't match"

                ( _, (Spread (Just name)) :: _ ) ->
                    Ok (( name, List remainingObj ) :: accResult)

                ( _, (Spread Nothing) :: _ ) ->
                    Ok accResult

                ( objItem :: objRest, patternItem :: patternRest ) ->
                    match ( objItem, patternItem )
                        |> Result.andThen
                            (\partialResult ->
                                matchHelper objRest patternRest (accResult ++ partialResult)
                            )

                _ ->
                    Err "Match failed: List sizes don't match"
    in
    matchHelper objItems patternItems []


type alias BinopHandler =
    Env -> Scrap -> Scrap -> Result String Scrap


bool : Bool -> Scrap
bool x =
    if x then
        true

    else
        false


binopHandlers : Dict String BinopHandler
binopHandlers =
    Dict.fromList
        [ ( "ADD"
          , \env x y ->
                Result.map2
                    (\a b -> wrapInferredNumberType (a + b))
                    (evalNumber env x)
                    (evalNumber env y)
          )
        , ( "SUB"
          , \env x y ->
                Result.map2
                    (\a b -> wrapInferredNumberType (a - b))
                    (evalNumber env x)
                    (evalNumber env y)
          )
        , ( "MUL"
          , \env x y ->
                Result.map2
                    (\a b -> wrapInferredNumberType (a * b))
                    (evalNumber env x)
                    (evalNumber env y)
          )
        , ( "DIV"
          , \env x y ->
                Result.map2
                    (\a b -> wrapInferredNumberType (a / b))
                    (evalNumber env x)
                    (evalNumber env y)
          )
        , ( "FLOOR_DIV"
          , \env x y ->
                Result.map2
                    (\a b -> wrapInferredNumberType (toFloat (floor (a / b))))
                    (evalNumber env x)
                    (evalNumber env y)
          )
        , ( "EXP"
          , \env x y ->
                Result.map2
                    (\a b -> wrapInferredNumberType (a ^ b))
                    (evalNumber env x)
                    (evalNumber env y)
          )
        , ( "MOD"
          , \env x y ->
                Result.map2
                    (\a b -> wrapInferredNumberType (toFloat (modBy (round b) (round a))))
                    (evalNumber env x)
                    (evalNumber env y)
          )
        , ( "EQUAL"
          , \env x y ->
                Result.map2
                    (\a b -> bool (a == b))
                    (eval ( env, x ))
                    (eval ( env, y ))
          )
        , ( "NOT_EQUAL"
          , \env x y ->
                Result.map2
                    (\a b -> bool (a /= b))
                    (eval ( env, x ))
                    (eval ( env, y ))
          )
        , ( "LESS"
          , \env x y ->
                Result.map2
                    (\a b -> bool (a < b))
                    (evalNumber env x)
                    (evalNumber env y)
          )
        , ( "GREATER"
          , \env x y ->
                Result.map2
                    (\a b -> bool (a > b))
                    (evalNumber env x)
                    (evalNumber env y)
          )
        , ( "LESS_EQUAL"
          , \env x y ->
                Result.map2
                    (\a b -> bool (a <= b))
                    (evalNumber env x)
                    (evalNumber env y)
          )
        , ( "GREATER_EQUAL"
          , \env x y ->
                Result.map2
                    (\a b -> bool (a >= b))
                    (evalNumber env x)
                    (evalNumber env y)
          )
        , ( "BOOL_AND"
          , \env x y ->
                Result.map2
                    (\a b -> bool (a && b))
                    (evalBool env x)
                    (evalBool env y)
          )
        , ( "BOOL_OR"
          , \env x y ->
                Result.map2
                    (\a b -> bool (a || b))
                    (evalBool env x)
                    (evalBool env y)
          )
        , ( "STRING_CONCAT"
          , \env x y ->
                Result.map2
                    (\a b -> String (a ++ b))
                    (evalString env x)
                    (evalString env y)
          )
        , ( "LIST_CONS"
          , \env x y ->
                Result.map2
                    (\a b -> List (a :: b))
                    (eval ( env, x ))
                    (evalList env y)
          )
        , ( "LIST_APPEND"
          , \env x y ->
                Result.map2
                    (\a b -> List (a ++ [ b ]))
                    (evalList env x)
                    (eval ( env, y ))
          )
        , ( "RIGHT_EVAL"
          , \env _ y ->
                eval ( env, y )
          )
        ]


wrapInferredNumberType : Float -> Scrap
wrapInferredNumberType x =
    if toFloat (round x) == x then
        Int (round x)

    else
        Float x


evalNumber : Env -> Scrap -> Result String Float
evalNumber env scrap =
    case eval ( env, scrap ) of
        Ok (Int i) ->
            Ok (toFloat i)

        Ok (Float f) ->
            Ok f

        _ ->
            Err "Expected a number"


evalBool : Env -> Scrap -> Result String Bool
evalBool env scrap =
    case eval ( env, scrap ) of
        Ok (Variant ( "true", Hole )) ->
            Ok True

        Ok (Variant ( "false", Hole )) ->
            Ok False

        _ ->
            Err "Expected a boolean"


evalString : Env -> Scrap -> Result String String
evalString env scrap =
    case eval ( env, scrap ) of
        Ok (String s) ->
            Ok s

        _ ->
            Err "Expected a string"


evalList : Env -> Scrap -> Result String (List Scrap)
evalList env scrap =
    case eval ( env, scrap ) of
        Ok (List items) ->
            Ok items

        _ ->
            Err "Expected a list"


binopKindToString : BinopKind -> String
binopKindToString kind =
    case kind of
        ADD ->
            "ADD"

        SUB ->
            "SUB"

        MUL ->
            "MUL"

        DIV ->
            "DIV"

        FLOOR_DIV ->
            "FLOOR_DIV"

        EXP ->
            "EXP"

        MOD ->
            "MOD"

        EQUAL ->
            "EQUAL"

        NOT_EQUAL ->
            "NOT_EQUAL"

        LESS ->
            "LESS"

        GREATER ->
            "GREATER"

        LESS_EQUAL ->
            "LESS_EQUAL"

        GREATER_EQUAL ->
            "GREATER_EQUAL"

        BOOL_AND ->
            "BOOL_AND"

        BOOL_OR ->
            "BOOL_OR"

        STRING_CONCAT ->
            "STRING_CONCAT"

        LIST_CONS ->
            "LIST_CONS"

        LIST_APPEND ->
            "LIST_APPEND"

        RIGHT_EVAL ->
            "RIGHT_EVAL"

        HASTYPE ->
            "HASTYPE"


eval : ( Env, Scrap ) -> Result String Scrap
eval ( env, exp ) =
    case exp of
        Int _ ->
            Ok exp

        Float _ ->
            Ok exp

        String _ ->
            Ok exp

        Bytes _ ->
            Ok exp

        Hole ->
            Ok exp

        Closure ( _, _ ) ->
            Ok exp

        NativeFunction _ ->
            Ok exp

        Variant ( tag, value ) ->
            eval ( env, value )
                |> Result.map (Variant << Tuple.pair tag)

        Var name ->
            Dict.get name env
                |> Result.fromMaybe ("Name '" ++ name ++ "' is not defined")

        List items ->
            List.foldr
                (\item accResult ->
                    Result.map2 (::) (eval ( env, item )) accResult
                )
                (Ok [])
                items
                |> Result.map List

        Record data ->
            List.foldr
                (\( key, value ) accResult ->
                    Result.map2
                        (\evaluatedValue acc ->
                            ( key, evaluatedValue ) :: acc
                        )
                        (eval ( env, value ))
                        accResult
                )
                (Ok [])
                data
                |> Result.map Record

        Function ( arg, body ) ->
            Ok (Closure ( env, Function ( arg, body ) ))

        MatchFunction cases ->
            Ok (Closure ( env, MatchFunction cases ))

        Apply ( func, arg ) ->
            Result.map2
                (\evaledFunc evaledArg ->
                    case evaledFunc of
                        NativeFunction nativeFunc ->
                            nativeFunc evaledArg

                        Closure ( closureEnv, closureFunc ) ->
                            case closureFunc of
                                Function ( Var argName, body ) ->
                                    eval ( Dict.insert argName evaledArg closureEnv, body )

                                MatchFunction matchCases ->
                                    applyMatchFunction closureEnv matchCases evaledArg

                                _ ->
                                    Err "TODO: unknown closure situation"

                        _ ->
                            Err ("Cannot apply non-function of type: " ++ Debug.toString evaledFunc)
                )
                (eval ( env, func ))
                (eval ( env, arg ))
                |> Result.andThen identity

        Access ( obj, at ) ->
            Result.map2
                (\evaledObj evaledAt ->
                    case evaledObj of
                        Record record ->
                            case evaledAt of
                                Var field ->
                                    Dict.get field (Dict.fromList record)
                                        |> Result.fromMaybe ("No assignment to " ++ field ++ " found in record")

                                _ ->
                                    Err "Cannot access record field using non-Var type"

                        List items ->
                            case evaledAt of
                                Int index ->
                                    getAt index items
                                        |> Result.fromMaybe ("Index " ++ String.fromInt index ++ " out of bounds for list")

                                _ ->
                                    Err "Cannot index into list using non-Int type"

                        _ ->
                            Err ("Cannot access from type: " ++ Debug.toString evaledObj)
                )
                (eval ( env, obj ))
                (eval ( env, at ))
                |> Result.andThen identity

        Spread _ ->
            Err "Cannot evaluate a spread"

        Binop kind ( left, right ) ->
            case Dict.get (binopKindToString kind) binopHandlers of
                Just handler ->
                    handler env left right

                Nothing ->
                    Err ("Unsupported binary operation: " ++ binopKindToString kind)

        exp_ ->
            Err ("TODO: unknown eval: " ++ Debug.toString exp_)


applyMatchFunction : Env -> List MatchCase -> Scrap -> Result String Scrap
applyMatchFunction env cases arg =
    let
        tryMatch : List MatchCase -> Result String Scrap
        tryMatch remainingCases =
            case remainingCases of
                [] ->
                    Err "No matching cases"

                (MatchCase ( pattern, body )) :: rest ->
                    match ( arg, pattern )
                        |> Result.andThen
                            (\matchResult ->
                                eval ( Dict.union (Dict.fromList matchResult) env, body )
                            )
                        |> Result.orElse (tryMatch rest)
    in
    tryMatch cases


getAt : Int -> List a -> Maybe a
getAt index list =
    list
        |> List.drop index
        |> List.head


bootEnv : Env
bootEnv =
    let
        stdlib =
            Dict.fromList
                [ ( "$$add"
                  , Closure ( Dict.empty, Function ( Var "x", Function ( Var "y", Binop ADD ( Var "x", Var "y" ) ) ) )
                  )
                , ( "$$listlength"
                  , NativeFunction
                        (\exp ->
                            case exp of
                                List xs ->
                                    Ok (Int (List.length xs))

                                _ ->
                                    Err "TODO: not a list"
                        )
                  )

                -- , ( "$$fetch", NativeFunction fetch )
                -- , ( "$$jsondecode", NativeFunction jsondecode )
                -- , ( "$$serialize", NativeFunction serialize )
                -- , ( "$$deserialize", NativeFunction deserialize )
                ]

        prelude =
            """
          id = x -> x
          . quicksort =
            | [] -> []
            | [p, ...xs] -> (concat ((quicksort (ltp xs p)) +< p) (quicksort (gtp xs p))
              . gtp = xs -> p -> filter (x -> x >= p) xs
              . ltp = xs -> p -> filter (x -> x < p) xs)
          . filter = f ->
            | [] -> []
            | [x, ...xs] -> f x |> | #true () -> x >+ filter f xs
                                   | #false () -> filter f xs
          . concat = xs ->
            | [] -> xs
            | [y, ...ys] -> concat (xs +< y) ys
          . map = f ->
            | [] -> []
            | [x, ...xs] -> f x >+ map f xs
          . range =
            | 0 -> []
            | i -> range (i - 1) +< (i - 1)
          . foldr = f -> a ->
            | [] -> a
            | [x, ...xs] -> f x (foldr f a xs)
          . take =
            | 0 -> xs -> []
            | n ->
              | [] -> []
              | [x, ...xs] -> x >+ take (n - 1) xs
          . all = f ->
            | [] -> #true ()
            | [x, ...xs] -> f x && all f xs
          . any = f ->
            | [] -> #false ()
            | [x, ...xs] -> f x || any f xs
          """
    in
    Dict.union stdlib Dict.empty



-- (parsePrelude prelude)


run : String -> Result String Scrap
run exp =
    case lex exp of
        Err tokenizeError ->
            Err ("Tokenization error: " ++ tokenizeError)

        Ok tokens ->
            case parse tokens of
                Err parseError ->
                    Err ("Parsing error: " ++ parseError)

                Ok ast ->
                    eval ( bootEnv, ast )
