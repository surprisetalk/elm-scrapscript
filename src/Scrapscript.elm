module Scrapscript exposing (..)

-- TODO: only exposing (Scrap, run, toString)

import Char
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Step(..), end, lazy, oneOf, succeed, symbol)
import Parser.Extras as Parser
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


parse : List Token -> Result String Scrap
parse tokens =
    Err "TODO: parse"


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
