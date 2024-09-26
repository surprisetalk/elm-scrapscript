module Scrapscript exposing (..)

-- TODO: only exposing (Scrap, run, toString)

import Char
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Step(..), end, lazy, oneOf, succeed, symbol)
import Parser.Extras as Parser
import Set exposing (Set)
import Dict exposing (Dict)
import Result exposing (Result)


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
     ( Parser.succeed identity
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
    Variant ( "true", List [] )


false : Scrap
false =
    Variant ( "false", List [] )


type alias Prec =
    { pl : Float
    , pr : Float
    }

type alias ParserState =
    { tokens : List Token
    , counter : Int
    }

ps : Dict String Prec
ps =
    Dict.fromList
        [ ( "::", { pl = 2000, pr = 2000 } )
        , ( "@", { pl = 1001, pr = 1001.1 } )
        , ( "", { pl = 1000, pr = 1000.1 } )
        -- ... (other operators omitted for brevity)
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
                        |> Result.map (\( obj, s ) -> ( Variant (value, obj), s ))

                BytesLit ( value, base ) ->
                    case base of
                        85 ->
                            Ok ( Bytes (value), newState )

                        64 ->
                            Ok ( Bytes value, newState )

                        32 ->
                            Ok ( Bytes (value), newState )

                        16 ->
                            Ok ( Bytes (value), newState )

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
                        (RightParen :: rest) ->
                            Ok ( Hole, { newState | tokens = rest } )

                        _ ->
                            parseHelper newState 0
                                |> Result.andThen (\( obj, s ) ->
                                    case s.tokens of
                                        (RightParen :: rest) ->
                                            Ok ( obj, { s | tokens = rest } )

                                        _ ->
                                            Err "TODO: unexpected token" -- (UnexpectedToken (Maybe.withDefault LeftParen (List.head s.tokens)))
                                )

                LeftBracket ->
                    parseList newState

                LeftBrace ->
                    parseRecord newState

                Operator "-" ->
                    parseHelper newState (highestPrec + 1)
                        |> Result.map (\( obj, s ) -> ( Binop SUB (Int 0, obj), s ))

                _ ->
                    Err "TODO: unexpected token" -- (UnexpectedToken token)

parseRecord : ParserState -> Result String ( Scrap, ParserState )
parseRecord state =
    let
        parseRecordHelper : ParserState -> List (String, Scrap) -> Result String ( Scrap, ParserState )
        parseRecordHelper s acc =
            case s.tokens of
                (RightBrace :: rest) ->
                    Ok ( Record acc, { s | tokens = rest } )

                _ ->
                    parseAssign s
                        |> Result.andThen (\s_ -> case s_ of
                            ( Assign ((Var name), value), newState ) ->
                              case newState.tokens of
                                  (RightBrace :: rest) ->
                                      Ok ( Record ((name, value) :: acc), { newState | tokens = rest } )

                                  (Operator "," :: rest) ->
                                      parseRecordHelper { newState | tokens = rest } ((name, value) :: acc)

                                  _ ->
                                      Err "TODO: unexpected token" -- (UnexpectedToken (Maybe.withDefault LeftBrace (List.head newState.tokens)))
                            _ ->
                                      Err ("TODO: unexpected record deconstruction")
                        )
    in
    parseRecordHelper state []

parseList : ParserState -> Result String ( Scrap, ParserState )
parseList state =
    let
        parseListHelper : ParserState -> List Scrap -> Result String ( Scrap, ParserState )
        parseListHelper s acc =
            case s.tokens of
                (RightBracket :: rest) ->
                    Ok ( List (List.reverse acc), { s | tokens = rest } )

                _ ->
                    parseHelper s 2
                        |> Result.andThen (\( item, newState ) ->
                            case newState.tokens of
                                (RightBracket :: rest) ->
                                    Ok ( List (List.reverse (item :: acc)), { newState | tokens = rest } )

                                (Operator "," :: rest) ->
                                    parseListHelper { newState | tokens = rest } (item :: acc)

                                _ ->
                                    Err "TODO: unexpected token" -- (UnexpectedToken (Maybe.withDefault LeftBracket (List.head newState.tokens)))
                        )
    in
    parseListHelper state []

parseAssign : ParserState -> Result String ( Scrap, ParserState )
parseAssign state =
    parseHelper state 0
        |> Result.andThen (\( obj, newState ) ->
            case obj of
                Spread _ ->
                    Ok ( Assign (Var "...", obj), newState )

                _ ->
                    case newState.tokens of
                        (Operator "=" :: rest) ->
                            parseHelper { newState | tokens = rest } (getPrec "=").pl
                                |> Result.map (\( value, s ) -> ( Assign (obj, value), s ))

                        _ ->
                            Err ("Failed to parse variable assignment in record constructor")
        )

parseMatchFunction : ParserState -> Result String ( Scrap, ParserState )
parseMatchFunction state =
    let
        parseCases : ParserState -> List MatchCase -> Result String ( Scrap, ParserState )
        parseCases s acc =
            parseHelper s (getPrec "|").pl
                |> Result.andThen (\( expr, newState ) ->
                    case expr of
                        Function (arg, body) ->
                            let
                                newCase =
                                    MatchCase (arg, body)
                            in
                            case newState.tokens of
                                (Operator "|" :: rest) ->
                                    parseCases { newState | tokens = rest } (newCase :: acc)

                                _ ->
                                    Ok ( MatchFunction (List.reverse (newCase :: acc)), newState )

                        _ ->
                            Err ("Expected function in match expression")
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

        (Operator op :: rest) ->
            let
                prec =
                    getPrec op
            in
            if prec.pl < minPrec then
                Ok ( left, state )
            else
                parseHelper { state | tokens = rest } prec.pr
                    |> Result.andThen (\( right, newState ) ->
                        case op of
                            "=" ->
                                case left of
                                    Var _ ->
                                        parseInfixAndApplication originalState minPrec ( Assign (left, right), newState )

                                    _ ->
                                        Err ("Expected variable in assignment")

                            "->" ->
                                parseInfixAndApplication originalState minPrec ( Function (left, right), newState )

                            "|>" ->
                                parseInfixAndApplication originalState minPrec ( Apply (right, left), newState )

                            "<|" ->
                                parseInfixAndApplication originalState minPrec ( Apply (left, right), newState )

                            ">>" ->
                                let
                                    ( varName, genState ) =
                                        gensym newState

                                    newFunc =
                                        Function ((Var varName), (Apply (right, (Apply (left, (Var varName))))))
                                in
                                parseInfixAndApplication originalState minPrec ( newFunc, genState )

                            "<<" ->
                                let
                                    ( varName, genState ) =
                                        gensym newState

                                    newFunc =
                                        Function ((Var varName), (Apply (left, (Apply (right, (Var varName))))))
                                in
                                parseInfixAndApplication originalState minPrec ( newFunc, genState )

                            "." ->
                                parseInfixAndApplication originalState minPrec ( Where (left, right), newState )

                            "?" ->
                                parseInfixAndApplication originalState minPrec ( Assert (left, right), newState )

                            "@" ->
                                parseInfixAndApplication originalState minPrec ( Access (left, right), newState )

                            _ ->
                                parseInfixAndApplication originalState minPrec ( Binop (toBinopKind op) (left, right), newState )
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
                    |> Result.andThen (\( right, newState ) ->
                        parseInfixAndApplication originalState minPrec ( Apply (left, right), newState )
                    )

toBinopKind : String -> BinopKind
toBinopKind op =
    case op of
        "+" -> ADD
        "-" -> SUB
        "*" -> MUL
        "/" -> DIV
        "%" -> MOD
        "&&" -> BOOL_AND
        "||" -> BOOL_OR
        "==" -> EQUAL
        "/=" -> NOT_EQUAL
        "<" -> LESS
        ">" -> GREATER
        "<=" -> LESS_EQUAL
        ">=" -> GREATER_EQUAL
        _ -> Debug.todo ("Unhandled operator: " ++ op)

match : ( Scrap, Scrap ) -> Result String (List ( String, Scrap ))
match ( exp, pattern ) =
    Err "TODO: match"


type alias Env =
    Dict String Scrap


eval : ( Env, Scrap ) -> Result String Scrap
eval ( env, exp ) =
    Err "TODO: eval"



{-
   , describe "EndToEndTestsBase"
     , test "_run" <| \ _ ->
           tokens = (lex text)
           ast = parse (tokens)
           if env is None:
               env = boot_env ()
           return eval (env, ast)
-}


run : String -> Result String Scrap
run exp =
    Err "TODO: run"



-- TODO


stdlib : String -> String
stdlib exp =
    exp
