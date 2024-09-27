module Scrapscript exposing (Scrap(..), parse, run)

import Bitwise
import Char
import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser, Problem, andThen, backtrackable, float, int, lazy, map, oneOf, spaces, succeed, symbol)
import Pratt as P
import Result.Extra as Result
import Set exposing (Set)


type alias Env =
    Dict String Scrap


type Scrap
    = Int Int
    | Float Float
    | String String
    | Bytes String
    | Var String
    | List (List Scrap)
    | Record (Dict String Scrap)
    | Function Scrap Scrap
    | Apply Scrap Scrap
    | Binop String Scrap Scrap
    | Hole
    | Variant String Scrap
    | Spread (Maybe Scrap)


true : Scrap
true =
    Variant "true" Hole


false : Scrap
false =
    Variant "false" Hole


eval : Env -> Scrap -> Result String Scrap
eval env scrap =
    case scrap of
        Bytes _ ->
            Ok scrap

        Hole ->
            Ok scrap

        Int _ ->
            Ok scrap

        Float _ ->
            Ok scrap

        String _ ->
            Ok scrap

        Var name ->
            Dict.get name env
                |> Result.fromMaybe ("Variable not found: " ++ name)

        List items ->
            List.foldr
                (\item accResult ->
                    Result.map2 (\acc evaluatedItem -> evaluatedItem :: acc)
                        accResult
                        (eval env item)
                )
                (Ok [])
                items
                |> Result.map List

        Record fields ->
            Dict.foldr
                (\key value accResult ->
                    Result.map2 (\acc evaluatedValue -> Dict.insert key evaluatedValue acc)
                        accResult
                        (eval env value)
                )
                (Ok Dict.empty)
                fields
                |> Result.map Record

        Function arg body ->
            Ok scrap

        Apply func arg ->
            Result.map2
                (\evaluatedFunc evaluatedArg ->
                    case evaluatedFunc of
                        Function (Var argName) body ->
                            eval (Dict.insert argName evaluatedArg env) body

                        _ ->
                            Err "Cannot apply non-function"
                )
                (eval env func)
                (eval env arg)
                |> Result.andThen identity

        Variant tag value ->
            eval env value
                |> Result.map (Variant tag)

        Binop "." body binding ->
            case binding of
                Binop "=" (Var var) value ->
                    eval env value
                        |> Result.andThen
                            (\evaluatedValue ->
                                eval (Dict.insert var evaluatedValue env) body
                            )

                _ ->
                    Err "Invalid binding in where expression"

        Binop "?" value cond ->
            eval env cond
                |> Result.andThen
                    (\evaluatedCond ->
                        if evaluatedCond == true then
                            eval env value

                        else
                            Err "Assertion failed"
                    )

        Spread _ ->
            Err "Cannot evaluate a spread directly"

        Binop op left right ->
            Result.map2
                (\evaluatedLeft evaluatedRight ->
                    case ( op, evaluatedLeft, evaluatedRight ) of
                        ( "+", Int a, Int b ) ->
                            Ok (Int (a + b))

                        ( "-", Int a, Int b ) ->
                            Ok (Int (a - b))

                        ( "*", Int a, Int b ) ->
                            Ok (Int (a * b))

                        ( "/", Int a, Int b ) ->
                            if b == 0 then
                                Err "Division by zero"

                            else
                                Ok (Float (toFloat a / toFloat b))

                        ( "++", String a, String b ) ->
                            Ok (String (a ++ b))

                        ( ">+", a, List b ) ->
                            Ok (List (a :: b))

                        ( "+<", List a, b ) ->
                            Ok (List (a ++ [ b ]))

                        -- ... (handle other cases)
                        _ ->
                            Err ("Unsupported operation: " ++ op)
                )
                (eval env left)
                (eval env right)
                |> Result.andThen identity


run : Env -> String -> Result String Scrap
run env =
    parse >> Result.andThen (eval env)


deadEndsToString : List P.DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : P.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Problem -> String
problemToString p =
    case p of
        P.Expecting s ->
            "expecting '" ++ s ++ "'"

        P.ExpectingInt ->
            "expecting int"

        P.ExpectingHex ->
            "expecting hex"

        P.ExpectingOctal ->
            "expecting octal"

        P.ExpectingBinary ->
            "expecting binary"

        P.ExpectingFloat ->
            "expecting float"

        P.ExpectingNumber ->
            "expecting number"

        P.ExpectingVariable ->
            "expecting variable"

        P.ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        P.ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        P.ExpectingEnd ->
            "expecting end"

        P.UnexpectedChar ->
            "unexpected char"

        P.Problem s ->
            "problem " ++ s

        P.BadRepeat ->
            "bad repeat"


parse : String -> Result String Scrap
parse input =
    P.run scrapParser input
        |> Result.mapError (\errors -> "Parse error: " ++ deadEndsToString errors)


space : Parser ()
space =
    P.chompIf (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}')


varParser : Parser String
varParser =
    -- TODO: Change to kebab case.
    P.variable
        { start = \c -> Char.isLower c || c == '$'
        , inner = \c -> Char.isAlphaNum c || c == '$' || c == '_' || c == '\''
        , reserved = Set.empty
        }


oneOfMany : List (P.Config Scrap -> Parser Scrap) -> List (P.Config Scrap -> Parser Scrap)
oneOfMany parsers =
    let
        parser : P.Config Scrap -> Parser Scrap
        parser config =
            List.map (\f -> f config) parsers |> oneOf
    in
    [ \config ->
        P.succeed (List.foldl Apply)
            |= parser config
            |= P.loop []
                (\terms ->
                    oneOf
                        [ succeed identity
                            |. space
                            |. spaces
                            |= oneOf
                                [ succeed (\term -> P.Loop (term :: terms))
                                    |= parser config
                                , succeed (P.Done terms)
                                ]
                        , succeed (P.Done terms)
                        ]
                )
    ]


scrapParser : Parser Scrap
scrapParser =
    P.succeed identity
        |. P.spaces
        |= P.expression
            { spaces = P.spaces
            , oneOf =
                oneOfMany
                    [ P.constant (P.keyword "()") Hole
                    , \config ->
                        P.succeed identity
                            |. P.symbol "("
                            |. spaces
                            |= P.subExpression 0 config
                            |. spaces
                            |. P.symbol ")"
                    , P.literal <|
                        P.succeed String
                            |. P.symbol "\""
                            |= P.getChompedString (P.chompWhile (\c -> c /= '"'))
                            |. P.symbol "\""
                    , P.literal <|
                        P.oneOf
                            [ P.succeed true |. P.keyword "true"
                            , P.succeed false |. P.keyword "false"
                            ]
                    , P.literal <|
                        P.succeed Bytes
                            |. P.symbol "~~"
                            |= P.getChompedString (P.chompWhile (\c -> c /= ' ' && c /= '\n'))
                    , \config ->
                        succeed Variant
                            |. symbol "#"
                            |. spaces
                            |= varParser
                            |. spaces
                            |= P.subExpression 7 config
                    , P.literal <|
                        P.succeed Var
                            |= varParser
                    , \config ->
                        P.sequence
                            { start = "["
                            , separator = ","
                            , end = "]"
                            , spaces = spaces
                            , item = P.subExpression 0 config
                            , trailing = P.Forbidden
                            }
                            |> P.map List
                    , \config ->
                        P.sequence
                            { start = "{"
                            , separator = ","
                            , end = "}"
                            , spaces = spaces
                            , item =
                                succeed Tuple.pair
                                    |= varParser
                                    |. spaces
                                    |. symbol "="
                                    |. spaces
                                    |= P.subExpression 0 config
                            , trailing = P.Forbidden
                            }
                            |> P.map (Dict.fromList >> Record)
                    , P.literal <|
                        -- supreme jank
                        P.andThen
                            (\x ->
                                case ( String.toInt x, String.toFloat x ) of
                                    ( Just n, _ ) ->
                                        P.succeed (Int n)

                                    ( _, Just n ) ->
                                        P.succeed (Float n)

                                    _ ->
                                        P.problem ("bad number: " ++ x)
                            )
                        <|
                            P.variable
                                { start = Char.isDigit
                                , inner = \c -> Char.isDigit c || c == '.'
                                , reserved = Set.empty
                                }

                    -- , P.prefix 10 (P.symbol "-") (Binop "-" (Int 0))
                    , P.constant (P.keyword "...") (Spread Nothing)
                    , \config ->
                        P.succeed (Spread << Just)
                            |= P.prefix 20 (P.symbol "...") identity config
                    ]
            , andThenOneOf =
                [ P.infixRight 2000 (P.symbol "::") (Binop "::")
                , P.infixLeft 16 (P.symbol ">>") (Binop ">>")
                , P.infixLeft 16 (P.symbol "<<") (Binop "<<")
                , P.infixLeft 14 (P.symbol "//") (Binop "//")
                , P.infixRight 12 (P.symbol ">*") (Binop ">*")
                , P.infixRight 12 (P.symbol "++") (Binop "++")
                , P.infixLeft 12 (P.symbol ">+") (Binop ">+")
                , P.infixRight 12 (P.symbol "+<") (Binop "+<")
                , P.infixLeft 11 (P.symbol "==") (Binop "==")
                , P.infixLeft 11 (P.symbol "/=") (Binop "/=")
                , P.infixLeft 11 (P.symbol "<=") (Binop "<=")
                , P.infixLeft 11 (P.symbol ">=") (Binop ">=")
                , P.infixRight 10 (P.symbol "&&") (Binop "&&")
                , P.infixRight 9 (P.symbol "||") (Binop "||")
                , P.infixRight 8 (P.symbol "|>") (Binop "|>")
                , P.infixLeft 8 (P.symbol "<|") (Binop "<|")
                , P.infixLeft 6 (P.symbol "->") (Binop "->")

                -- single chars after multichars
                , P.infixRight 1001 (P.symbol "@") (Binop "@")
                , P.infixRight 15 (P.symbol "^") (Binop "^")
                , P.infixRight 14 (P.symbol "*") (Binop "*")
                , P.infixRight 14 (P.symbol "/") (Binop "/")
                , P.infixLeft 14 (P.symbol "%") (Binop "%")
                , P.infixLeft 13 (P.symbol "+") (Binop "+")
                , P.infixLeft 13 (P.symbol "-") (Binop "-")
                , P.infixLeft 11 (P.symbol "<") (Binop "<")
                , P.infixLeft 11 (P.symbol ">") (Binop ">")
                , P.infixLeft 7 (P.symbol "#") (Binop "#")
                , P.infixRight 5 (P.symbol "|") (Binop "|")
                , P.infixLeft 5 (P.symbol ":") (Binop ":")
                , P.infixRight 4 (P.symbol "=") (Binop "=")
                , P.infixLeft 3 (P.symbol "!") (Binop "!")
                , P.infixRight 3 (P.symbol ".") (Binop ".")
                , P.infixRight 3 (P.symbol "?") (Binop "?")
                ]
            }
        |. P.spaces
        |. P.end
