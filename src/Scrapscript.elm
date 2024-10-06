module Scrapscript exposing (Scrap(..), parse, run, toString)

import Array
import Char
import Dict exposing (Dict)
import Html exposing (b)
import Parser as P exposing ((|.), (|=), Parser, Problem, andThen, backtrackable, float, int, lazy, map, oneOf, succeed, symbol)
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
    | Record (List ( String, Scrap ))
    | Apply Scrap Scrap
    | Binop String Scrap Scrap
    | Variant String Scrap
    | Spread (Maybe Scrap)
    | Match (List Scrap)
    | Native (Scrap -> Result String Scrap)
    | Closure Env Scrap Scrap


toString : Scrap -> String
toString exp =
    case exp of
        Hole ->
            "()"

        Int x ->
            String.fromInt x

        Float x ->
            String.fromFloat x

        Text x ->
            x

        Bytes _ ->
            "TODO: bytes"

        Var x ->
            x

        List x ->
            "[" ++ String.join ", " (List.map toString x) ++ "]"

        Record x ->
            "{" ++ String.join ", " (Dict.foldl (\k v s -> (k ++ " = " ++ toString v) :: s) [] (Dict.fromList x)) ++ "}"

        Apply f x ->
            toString f ++ " " ++ toString x

        Binop op a b ->
            toString a ++ " " ++ op ++ " " ++ toString b

        Variant k Hole ->
            "#" ++ k

        Variant k v ->
            "#" ++ k ++ " " ++ toString v

        Spread Nothing ->
            "..."

        Spread (Just x) ->
            "..." ++ toString x

        Match xs ->
            xs |> List.map (\x -> "| " ++ toString x) |> String.join " "

        Native _ ->
            "TODO: native"

        Closure _ a b ->
            "(" ++ toString a ++ " -> " ++ toString b ++ ")"


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
    -- TODO: Implement type-checker.
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
            items
                |> List.map (eval env)
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> Result.map List

        Record fields ->
            fields
                |> List.map (\( k, v ) -> eval env v |> Result.map (Tuple.pair k))
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> Result.map Record

        Binop "->" arg body ->
            Ok (Closure env arg body)

        Closure env_ arg body ->
            Ok (Closure env_ arg body)

        Match cases ->
            Ok (Match cases)

        Apply (Native f) arg ->
            eval env arg |> Result.andThen f

        Apply func arg ->
            Result.map2
                (\evaluatedFunc evaluatedArg ->
                    -- TODO: Implement Match too.
                    case evaluatedFunc of
                        Binop "->" (Var argName) body ->
                            eval (Dict.insert argName evaluatedArg env) body

                        Closure env_ (Var argName) body ->
                            eval (Dict.insert argName evaluatedArg env_) body

                        f ->
                            Err ("Cannot apply non-function: " ++ toString (Apply f evaluatedArg))
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

        Native _ ->
            Err "Cannot evaluate a native function directly"

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

                        ( "//", Int a, Int b ) ->
                            if b == 0 then
                                Err "Division by zero"

                            else
                                Ok (Int (a // b))

                        ( "/", Int a, Int b ) ->
                            if b == 0 then
                                Err "Division by zero"

                            else
                                Ok (Float (toFloat a / toFloat b))

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

                        ( "++", Text a, Text b ) ->
                            Err "Text concatenation not allowed. Please use <text/concat a b> or <\"`a``b`\">."

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
                            eval env (Apply b a)

                        ( "<|", a, b ) ->
                            eval env (Apply a b)

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
        -- TODO: Implement much nicer errors.
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
            -- List.map (\deadend -> problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col) >> String.join "; "
            List.head
                >> Maybe.map
                    (\deadend ->
                        input
                            |> String.split "\n"
                            |> Array.fromList
                            |> Array.get (deadend.row - 1)
                            |> Maybe.map (String.dropLeft (deadend.col - 1))
                            |> Maybe.withDefault (problemToString deadend.problem)
                    )
                >> Maybe.withDefault "TODO: something went wrong"
    in
    P.run scrap input
        |> Result.mapError (\errors -> "Parse error: " ++ deadEndsToString errors)


scrap : Parser Scrap
scrap =
    let
        spaces : Parser ()
        spaces =
            -- TODO: Implement line comments.
            P.spaces

        space : Parser ()
        space =
            -- TODO: Implement line comments.
            P.chompIf (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}')

        var : Parser String
        var =
            P.variable
                { start = \c -> Char.isLower c || c == '$'
                , inner = \c -> Char.isAlphaNum c || c == '$' || c == '-' || c == '\'' || c == '/'
                , reserved = Set.empty
                }

        symbolUnless : String -> List String -> Parser ()
        symbolUnless yes nos =
            succeed identity
                |. backtrackable (symbol yes)
                |= oneOf (List.map (symbol >> backtrackable >> P.map (always True)) nos ++ [ succeed False ])
                |> andThen
                    (\isBadEnding ->
                        if isBadEnding then
                            P.problem ("expecting `" ++ yes ++ "` unfollowed by `" ++ String.join ", " nos ++ "`")

                        else
                            P.commit ()
                    )

        minus : Parser ()
        minus =
            symbolUnless "-" [ ">", "-" ]

        bar : Parser ()
        bar =
            symbolUnless "|" [ "|", ">" ]

        flip : (a -> b -> c) -> (b -> a -> c)
        flip f a b =
            f b a

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
                P.succeed (\( x, xs ) -> List.foldl (flip Apply) x xs)
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
                            -- TODO: Implement escaping, e.g. "\n".
                            -- TODO: Implement string interpolation, e.g. "hello `name`".
                            -- TODO: Strings should be multiline without """.
                            |. P.symbol "\""
                            |= P.getChompedString (P.chompWhile (\c -> c /= '"'))
                            |. P.symbol "\""
                    , P.literal <|
                        P.oneOf
                            [ P.succeed true |. P.keyword "true"
                            , P.succeed false |. P.keyword "false"
                            ]
                    , P.literal <|
                        -- TODO: Implement byte conversion.
                        P.succeed Bytes
                            |. P.symbol "~~"
                            |= P.getChompedString (P.chompWhile (\c -> c /= ' ' && c /= '\n'))
                    , \config ->
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
                                oneOf
                                    [ succeed (\k -> ( "..." ++ Maybe.withDefault "" k, Spread (Maybe.map Var k) ))
                                        |. P.symbol "..."
                                        |= oneOf
                                            [ succeed Just |= var
                                            , succeed Nothing
                                            ]
                                    , succeed (\k v -> ( k, Maybe.withDefault (Var k) v ))
                                        |= var
                                        |. spaces
                                        |= oneOf
                                            [ succeed Just
                                                |. symbol "="
                                                |. spaces
                                                |= P.subExpression 0 config
                                            , P.succeed Nothing
                                            ]
                                    ]
                            , trailing = P.Forbidden
                            }
                            |> P.map Record
                    , P.literal <|
                        -- TODO: Redo this supreme jank.
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
                        -- TODO: Something is very broken here.
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
                -- TODO: The "@" symbol is too symbolically noisy for record access.
                -- TODO:   Perhaps we should use `.` instead and let variables have their own little sub-language?
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
                , P.infixLeft 8 (P.symbol "|>") (Binop "|>")
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
