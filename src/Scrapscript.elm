module Scrapscript exposing (Scrap(..), parse, run)

import Char
import Dict exposing (Dict)
import Html exposing (b)
import Parser as P exposing ((|.), (|=), Parser, Problem, andThen, backtrackable, float, int, lazy, map, oneOf, spaces, succeed, symbol)
import Pratt as P
import Set


type alias Env =
    Dict String Scrap


type Scrap
    = Hole
    | Int Int
    | Float Float
    | Text String
    | Bytes String
    | Var String
    | List (List Scrap)
    | Record (Dict String Scrap)
    | Apply Scrap Scrap
    | Binop String Scrap Scrap
    | Variant String Scrap
    | Spread (Maybe Scrap)
    | Match (List Scrap)


bool : Bool -> Scrap
bool x =
    if x then
        true

    else
        false


true : Scrap
true =
    Variant "true" Hole


false : Scrap
false =
    Variant "false" Hole


check : Env -> Scrap -> Result String ( Env, Scrap )
check env x =
    -- TODO
    Ok ( Dict.empty, x )


eval : Env -> Scrap -> Result String Scrap
eval env exp =
    case exp of
        Bytes _ ->
            Ok exp

        Hole ->
            Ok exp

        Int _ ->
            Ok exp

        Float _ ->
            Ok exp

        Text _ ->
            Ok exp

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

        Binop "->" arg body ->
            Ok (Binop "->" arg body)

        Match cases ->
            Ok (Match cases)

        Apply func arg ->
            Result.map2
                (\evaluatedFunc evaluatedArg ->
                    case evaluatedFunc of
                        Binop "->" (Var argName) body ->
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

        Binop "." body (Binop "=" (Var var) value) ->
            eval env value
                |> Result.andThen
                    (\evaluatedValue ->
                        eval (Dict.insert var evaluatedValue env) body
                    )

        Binop "." _ _ ->
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

                        ( "+", Float a, Float b ) ->
                            Ok (Float (a + b))

                        ( "-", Float a, Float b ) ->
                            Ok (Float (a - b))

                        ( "*", Float a, Float b ) ->
                            Ok (Float (a * b))

                        ( "/", Int a, Int b ) ->
                            if b == 0 then
                                Err "Division by zero"

                            else
                                Ok (Int (a // b))

                        ( "/", Float a, Float b ) ->
                            if b == 0 then
                                Err "Division by zero"

                            else
                                Ok (Float (a / b))

                        ( ">+", a, List b ) ->
                            Ok (List (a :: b))

                        ( "+<", List a, b ) ->
                            Ok (List (a ++ [ b ]))

                        ( "++", List a, List b ) ->
                            Ok (List (a ++ b))

                        ( "==", a, b ) ->
                            Ok (bool (a == b))

                        ( "/=", a, b ) ->
                            Ok (bool (a /= b))

                        ( "<=", Int a, Int b ) ->
                            Ok (bool (a <= b))

                        ( ">=", Int a, Int b ) ->
                            Ok (bool (a >= b))

                        ( "<=", Float a, Float b ) ->
                            Ok (bool (a <= b))

                        ( ">=", Float a, Float b ) ->
                            Ok (bool (a >= b))

                        ( "<=", Text a, Text b ) ->
                            Ok (bool (a <= b))

                        ( ">=", Text a, Text b ) ->
                            Ok (bool (a >= b))

                        ( "<", Int a, Int b ) ->
                            Ok (bool (a < b))

                        ( ">", Int a, Int b ) ->
                            Ok (bool (a > b))

                        ( "<", Float a, Float b ) ->
                            Ok (bool (a < b))

                        ( ">", Float a, Float b ) ->
                            Ok (bool (a > b))

                        ( "<", Text a, Text b ) ->
                            Ok (bool (a < b))

                        ( ">", Text a, Text b ) ->
                            Ok (bool (a > b))

                        ( "&&", Variant a Hole, Variant b Hole ) ->
                            Ok (bool (a == "true" && b == "true"))

                        ( "||", Variant a Hole, Variant b Hole ) ->
                            Ok (bool (a == "true" || b == "true"))

                        ( "|>", a, b ) ->
                            Ok (Apply b a)

                        ( "<|", a, b ) ->
                            Ok (Apply a b)

                        ( "^", Int a, Int b ) ->
                            Ok (Int (a ^ b))

                        ( "^", Float a, Float b ) ->
                            Ok (Float (a ^ b))

                        ( "%", Int a, Int b ) ->
                            Ok (Int (modBy b a))

                        _ ->
                            Err ("Unsupported operation: " ++ op)
                )
                (eval env left)
                (eval env right)
                |> Result.andThen identity


run : Env -> String -> Result String Scrap
run env =
    parse
        >> Result.andThen (check env)
        >> Result.andThen (Tuple.second >> eval env)


parse : String -> Result String Scrap
parse input =
    let
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

        deadEndsToString : List P.DeadEnd -> String
        deadEndsToString =
            List.map (\deadend -> problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col)
                >> String.join "; "
    in
    P.run scrap input
        |> Result.mapError (\errors -> "Parse error: " ++ deadEndsToString errors)


scrap : Parser Scrap
scrap =
    let
        space : Parser ()
        space =
            P.chompIf (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}')

        var : Parser String
        var =
            -- TODO: Change to kebab case.
            P.variable
                { start = \c -> Char.isLower c || c == '$'
                , inner = \c -> Char.isAlphaNum c || c == '$' || c == '_' || c == '\''
                , reserved = Set.empty
                }

        symbolUnless : String -> List String -> Parser ()
        symbolUnless yes nos =
            succeed identity
                |. backtrackable (symbol yes)
                |= oneOf (succeed False :: List.map (symbol >> backtrackable >> P.map (always True)) nos)
                |> andThen
                    (\isBadEnding ->
                        if isBadEnding then
                            P.problem ("expecting `" ++ yes ++ "` unfollowed by `" ++ String.join ", " nos ++ "`")

                        else
                            P.commit ()
                    )

        minus : Parser ()
        minus =
            symbolUnless "-" [ ">" ]

        bar : Parser ()
        bar =
            symbolUnless "|" [ "|", ">" ]

        some : Parser () -> Parser a -> Parser ( a, List a )
        some s p =
            P.succeed Tuple.pair
                |= p
                |= P.loop []
                    (\xs ->
                        oneOf
                            [ succeed identity
                                |. s
                                |= oneOf
                                    [ succeed (\x -> P.Loop (x :: xs))
                                        |= p
                                    , succeed (P.Done (List.reverse xs))
                                    ]
                            , succeed (P.Done (List.reverse xs))
                            ]
                    )

        oneOfMany : List (P.Config Scrap -> Parser Scrap) -> List (P.Config Scrap -> Parser Scrap)
        oneOfMany parsers =
            [ \config ->
                P.succeed (\( x, xs ) -> List.foldr Apply x xs)
                    |= some (space |. spaces) (oneOf <| List.map (\f -> f config) parsers)
            ]
    in
    P.succeed identity
        |. P.spaces
        |= P.expression
            { spaces = P.spaces
            , oneOf =
                oneOfMany
                    [ \config ->
                        P.succeed identity
                            |. P.symbol "("
                            |. spaces
                            |= oneOf
                                [ P.succeed Hole
                                    |. P.symbol ")"
                                , P.subExpression 0 config
                                    |. spaces
                                    |. P.symbol ")"
                                ]
                    , P.literal <|
                        P.succeed Text
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
                        -- TODO: some
                        succeed Variant
                            |. symbol "#"
                            |. spaces
                            |= var
                            |. spaces
                            |= oneOf
                                [ P.subExpression 7 config
                                , P.succeed Hole
                                ]
                    , P.literal <|
                        P.succeed Var
                            |= var
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
                                    |= var
                                    |. spaces
                                    |. symbol "="
                                    |. spaces
                                    |= P.subExpression 0 config
                            , trailing = P.Forbidden
                            }
                            |> P.map (Dict.fromList >> Record)
                    , P.literal <|
                        -- TODO: redo this supreme jank
                        P.andThen
                            (\x ->
                                case ( String.contains "." x, String.toInt x, String.toFloat x ) of
                                    ( True, _, Just n ) ->
                                        P.succeed (Float n)

                                    ( _, Just n, _ ) ->
                                        P.succeed (Int n)

                                    _ ->
                                        P.problem ("bad number: " ++ x)
                            )
                        <|
                            P.variable
                                { start = Char.isDigit
                                , inner = \c -> Char.isDigit c || c == '.'
                                , reserved = Set.empty
                                }
                    , \config ->
                        P.succeed (\( x, xs ) -> Match (x :: xs))
                            |= some spaces
                                (P.succeed identity
                                    |. P.prefix 4 bar identity config
                                    |. spaces
                                    |= P.subExpression 5 config
                                    |. spaces
                                )
                    , P.prefix 20 minus <|
                        \n_ ->
                            case n_ of
                                Int n ->
                                    Int -n

                                Float n ->
                                    Float -n

                                n ->
                                    Binop "-" (Int 0) n
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
                , P.infixRight 6 (P.symbol "->") (Binop "->")
                , P.infixRight 1001 (P.symbol "@") (Binop "@")
                , P.infixRight 15 (P.symbol "^") (Binop "^")
                , P.infixRight 14 (P.symbol "*") (Binop "*")
                , P.infixRight 14 (P.symbol "/") (Binop "/")
                , P.infixLeft 14 (P.symbol "%") (Binop "%")
                , P.infixLeft 13 (P.symbol "+") (Binop "+")
                , P.infixLeft 13 minus (Binop "-")
                , P.infixLeft 11 (P.symbol "<") (Binop "<")
                , P.infixLeft 11 (P.symbol ">") (Binop ">")
                , P.infixLeft 5 (P.symbol ":") (Binop ":")
                , P.infixRight 4 (P.symbol "=") (Binop "=")
                , P.infixLeft 3 (P.symbol "!") (Binop "!")
                , P.infixRight 3 (P.symbol ".") (Binop ".")
                , P.infixRight 3 (P.symbol "?") (Binop "?")
                ]
            }
        |. P.spaces
        |. P.end
