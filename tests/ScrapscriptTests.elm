module ScrapscriptTests exposing (suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Scrapscript exposing (Scrap(..), run)
import Test exposing (..)


suite : Test
suite =
    describe "End-to-End Tests"
        [ test "int returns int" <|
            \_ ->
                run "1"
                    |> Expect.equal (Ok (Int 1))
        , test "float returns float" <|
            \_ ->
                run "3.14"
                    |> Expect.equal (Ok (Float 3.14))
        , test "bytes returns bytes" <|
            \_ ->
                run "~~QUJD"
                    |> Expect.equal (Ok (Bytes "QUJD"))
        , test "int add returns int" <|
            \_ ->
                run "1 + 2"
                    |> Expect.equal (Ok (Int 3))
        , test "int sub returns int" <|
            \_ ->
                run "1 - 2"
                    |> Expect.equal (Ok (Int -1))
        , test "string concat returns string" <|
            \_ ->
                run "\"abc\" ++ \"def\""
                    |> Expect.equal (Ok (String "abcdef"))
        , test "list cons returns list" <|
            \_ ->
                run "1 >+ [2,3]"
                    |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3 ]))
        , test "list cons nested returns list" <|
            \_ ->
                run "1 >+ 2 >+ [3,4]"
                    |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3, Int 4 ]))
        , test "list append returns list" <|
            \_ ->
                run "[1,2] +< 3"
                    |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3 ]))
        , test "list append nested returns list" <|
            \_ ->
                run "[1,2] +< 3 +< 4"
                    |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3, Int 4 ]))
        , test "empty list" <|
            \_ ->
                run "[ ]"
                    |> Expect.equal (Ok (List []))
        , test "empty list with no spaces" <|
            \_ ->
                run "[]"
                    |> Expect.equal (Ok (List []))
        , test "list of ints" <|
            \_ ->
                run "[ 1 , 2 ]"
                    |> Expect.equal (Ok (List [ Int 1, Int 2 ]))
        , test "list of exprs" <|
            \_ ->
                run "[ 1 + 2 , 3 + 4 ]"
                    |> Expect.equal (Ok (List [ Int 3, Int 7 ]))
        , test "where" <|
            \_ ->
                run "a + 2 . a = 1"
                    |> Expect.equal (Ok (Int 3))
        , test "nested where" <|
            \_ ->
                run "a + b . a = 1 . b = 2"
                    |> Expect.equal (Ok (Int 3))
        , test "assert with truthy cond returns value" <|
            \_ ->
                run "a + 1 ? a == 1 . a = 1"
                    |> Expect.equal (Ok (Int 2))
        , test "assert with falsey cond raises assertion error" <|
            \_ ->
                run "a + 1 ? a == 2 . a = 1"
                    |> Expect.err
        , test "nested assert" <|
            \_ ->
                run "a + b ? a == 1 ? b == 2 . a = 1 . b = 2"
                    |> Expect.equal (Ok (Int 3))
        , test "hole" <|
            \_ ->
                run "()"
                    |> Expect.equal (Ok Hole)
        , test "bindings behave like letstar" <|
            \_ ->
                run "b . a = 1 . b = a"
                    |> Expect.err
        , test "function application two args" <|
            \_ ->
                run "(a -> b -> a + b) 3 2"
                    |> Expect.equal (Ok (Int 5))
        , test "function create list correct order" <|
            \_ ->
                run "(a -> b -> [a, b]) 3 2"
                    |> Expect.equal (Ok (List [ Int 3, Int 2 ]))
        , test "create record" <|
            \_ ->
                run "{a = 1 + 3}"
                    |> Expect.equal (Ok (Record (Dict.fromList [ ( "a", Int 4 ) ])))
        , test "access record" <|
            \_ ->
                run "rec@b . rec = { a = 1, b = \"x\" }"
                    |> Expect.equal (Ok (String "x"))
        , test "access list" <|
            \_ ->
                run "xs@1 . xs = [1, 2, 3]"
                    |> Expect.equal (Ok (Int 2))
        , test "access list var" <|
            \_ ->
                run "xs@y . y = 2 . xs = [1, 2, 3]"
                    |> Expect.equal (Ok (Int 3))
        , test "access list expr" <|
            \_ ->
                run "xs@(1+1) . xs = [1, 2, 3]"
                    |> Expect.equal (Ok (Int 3))
        , test "access list closure var" <|
            \_ ->
                run "list_at 1 [1,2,3] . list_at = idx -> ls -> ls@idx"
                    |> Expect.equal (Ok (Int 2))
        , test "functions eval arguments" <|
            \_ ->
                run "(x -> x) c . c = 1"
                    |> Expect.equal (Ok (Int 1))
        , test "non var function arg raises parse error" <|
            \_ ->
                run "1 -> a"
                    |> Expect.err
        , test "compose" <|
            \_ ->
                run "((a -> a + 1) >> (b -> b * 2)) 3"
                    |> Expect.equal (Ok (Int 8))
        , test "compose does not expose internal x" <|
            \_ ->
                run "f 3 . f = ((y -> x) >> (z -> x))"
                    |> Expect.err
        , test "double compose" <|
            \_ ->
                run "((a -> a + 1) >> (x -> x) >> (b -> b * 2)) 3"
                    |> Expect.equal (Ok (Int 8))
        , test "reverse compose" <|
            \_ ->
                run "((a -> a + 1) << (b -> b * 2)) 3"
                    |> Expect.equal (Ok (Int 7))
        , test "simple int match" <|
            \_ ->
                run """
                inc 2
                . inc =
                  | 1 -> 2
                  | 2 -> 3
                """
                    |> Expect.equal (Ok (Int 3))
        , test "match var binds var" <|
            \_ ->
                run """
                id 3
                . id =
                  | x -> x
                """
                    |> Expect.equal (Ok (Int 3))
        , test "match var binds first arm" <|
            \_ ->
                run """
                id 3
                . id =
                  | x -> x
                  | y -> y * 2
                """
                    |> Expect.equal (Ok (Int 3))
        , test "match function can close over variables" <|
            \_ ->
                run """
                f 1 2
                . f = a ->
                  | b -> a + b
                """
                    |> Expect.equal (Ok (Int 3))
        , test "match record binds var" <|
            \_ ->
                run """
                get_x rec
                . rec = { x = 3 }
                . get_x =
                  | { x = x } -> x
                """
                    |> Expect.equal (Ok (Int 3))
        , test "match record binds vars" <|
            \_ ->
                run """
                mult rec
                . rec = { x = 3, y = 4 }
                . mult =
                  | { x = x, y = y } -> x * y
                """
                    |> Expect.equal (Ok (Int 12))
        , test "match record with extra fields does not match" <|
            \_ ->
                run """
                mult rec
                . rec = { x = 3 }
                . mult =
                  | { x = x, y = y } -> x * y
                """
                    |> Expect.err
        , test "match record with constant" <|
            \_ ->
                run """
                mult rec
                . rec = { x = 4, y = 5 }
                . mult =
                  | { x = 3, y = y } -> 1
                  | { x = 4, y = y } -> 2
                """
                    |> Expect.equal (Ok (Int 2))
        , test "match record with non record fails" <|
            \_ ->
                run """
                get_x 3
                . get_x =
                  | { x = x } -> x
                """
                    |> Expect.err
        , test "match record doubly binds vars" <|
            \_ ->
                run """
                get_x rec
                . rec = { a = 3, b = 3 }
                . get_x =
                  | { a = x, b = x } -> x
                """
                    |> Expect.equal (Ok (Int 3))
        , test "match record spread binds spread" <|
            \_ ->
                run "(| { a=1, ...rest } -> rest) {a=1, b=2, c=3}"
                    |> Expect.equal (Ok (Record (Dict.fromList [ ( "b", Int 2 ), ( "c", Int 3 ) ])))
        , test "match list binds vars" <|
            \_ ->
                run """
                mult xs
                . xs = [1, 2, 3, 4, 5]
                . mult =
                  | [1, x, 3, y, 5] -> x * y
                """
                    |> Expect.equal (Ok (Int 8))
        , test "match list incorrect length does not match" <|
            \_ ->
                run """
                mult xs
                . xs = [1, 2, 3]
                . mult =
                  | [1, 2] -> 1
                  | [1, 2, 3, 4] -> 1
                  | [1, 3] -> 1
                """
                    |> Expect.err
        , test "match list with constant" <|
            \_ ->
                run """
                middle xs
                . xs = [4, 5, 6]
                . middle =
                  | [1, x, 3] -> x
                  | [4, x, 6] -> x
                  | [7, x, 9] -> x
                """
                    |> Expect.equal (Ok (Int 5))
        , test "match list with non list fails" <|
            \_ ->
                run """
                get_x 3
                . get_x =
                  | [2, x] -> x
                """
                    |> Expect.err
        , test "match list doubly binds vars" <|
            \_ ->
                run """
                mult xs
                . xs = [1, 2, 3, 2, 1]
                . mult =
                  | [1, x, 3, x, 1] -> x
                """
                    |> Expect.equal (Ok (Int 2))
        , test "match list spread binds spread" <|
            \_ ->
                run "(| [x, ...xs] -> xs) [1, 2]"
                    |> Expect.equal (Ok (List [ Int 2 ]))
        , test "pipe" <|
            \_ ->
                run "1 |> (a -> a + 2)"
                    |> Expect.equal (Ok (Int 3))
        , test "pipe nested" <|
            \_ ->
                run "1 |> (a -> a + 2) |> (b -> b * 2)"
                    |> Expect.equal (Ok (Int 6))
        , test "reverse pipe" <|
            \_ ->
                run "(a -> a + 2) <| 1"
                    |> Expect.equal (Ok (Int 3))
        , test "reverse pipe nested" <|
            \_ ->
                run "(b -> b * 2) <| (a -> a + 2) <| 1"
                    |> Expect.equal (Ok (Int 6))

        {-
           , test "function can reference itself" <|
               \_ ->
                   run """
                   f 1
                   . f = n -> f
                   """
                       |> Expect.equal (Ok (Closure (Dict.singleton "f" (Var "f")) (Function (Var "n") (Var "f"))))
        -}
        , test "function can call itself" <|
            \_ ->
                run """
                f 1
                . f = n -> f n
                """
                    |> Expect.err
        , test "match function can call itself" <|
            \_ ->
                run """
                fac 5
                . fac =
                  | 0 -> 1
                  | 1 -> 1
                  | n -> n * fac (n - 1)
                """
                    |> Expect.equal (Ok (Int 120))
        , test "list access binds tighter than append" <|
            \_ ->
                run "[1, 2, 3] +< xs@0 . xs = [4]"
                    |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3, Int 4 ]))
        , test "exponentiation" <|
            \_ ->
                run "6 ^ 2"
                    |> Expect.equal (Ok (Int 36))
        , test "modulus" <|
            \_ ->
                run "11 % 3"
                    |> Expect.equal (Ok (Int 2))
        , test "exp binds tighter than mul" <|
            \_ ->
                run "5 * 2 ^ 3"
                    |> Expect.equal (Ok (Int 40))
        , test "variant true returns true" <|
            \_ ->
                run "# true ()"
                    |> Expect.equal (Ok (Variant "true" Hole))
        , test "variant false returns false" <|
            \_ ->
                run "#false ()"
                    |> Expect.equal (Ok (Variant "false" Hole))
        , test "boolean and binds tighter than or" <|
            \_ ->
                run "#true () || #true () && boom"
                    |> Expect.equal (Ok (Variant "true" Hole))
        , test "compare binds tighter than boolean and" <|
            \_ ->
                run "1 < 2 && 2 < 1"
                    |> Expect.equal (Ok (Variant "false" Hole))
        , test "match list spread" <|
            \_ ->
                run """
                f [2, 4, 6]
                . f =
                  | [] -> 0
                  | [x, ...] -> x
                  | c -> 1
                """
                    |> Expect.equal (Ok (Int 2))
        , test "match list named spread" <|
            \_ ->
                run """
                tail [1,2,3]
                . tail =
                  | [first, ...rest] -> rest
                """
                    |> Expect.equal (Ok (List [ Int 2, Int 3 ]))
        , test "match record spread" <|
            \_ ->
                run """
                f {x = 4, y = 5}
                . f =
                  | {} -> 0
                  | {x = a, ...} -> a
                  | c -> 1
                """
                    |> Expect.equal (Ok (Int 4))
        , test "match expr as boolean variants" <|
            \_ ->
                run """
                say (1 < 2)
                . say =
                  | #false () -> "oh no"
                  | #true () -> "omg"
                """
                    |> Expect.equal (Ok (String "omg"))
        , test "match variant record" <|
            \_ ->
                run """
                f #add {x = 3, y = 4}
                . f =
                  | # b () -> "foo"
                  | #add {x = x, y = y} -> x + y
                """
                    |> Expect.equal (Ok (Int 7))
        , test "multiple where clauses" <|
            \_ ->
                run """
                x + y + z
                . x = 1
                . y = 2
                . z = 3
                """
                    |> Expect.equal (Ok (Int 6))
        , test "nested functions" <|
            \_ ->
                run """
                outer 5
                . outer = x ->
                    inner (x + 1)
                    . inner = y -> y * 2
                """
                    |> Expect.equal (Ok (Int 12))
        , test "higher order functions" <|
            \_ ->
                run """
                apply (x -> x * 2) 3
                . apply = f -> x -> f x
                """
                    |> Expect.equal (Ok (Int 6))
        , test "currying" <|
            \_ ->
                run """
                add 2 3
                . add = x -> y -> x + y
                """
                    |> Expect.equal (Ok (Int 5))
        , test "complex list operations" <|
            \_ ->
                run """
                map (x -> x * 2) [1, 2, 3]
                . map = f ->
                  | [] -> []
                  | [x, ...xs] -> f x >+ map f xs
                """
                    |> Expect.equal (Ok (List [ Int 2, Int 4, Int 6 ]))
        , test "record update" <|
            \_ ->
                run """
                update_x {x = 1, y = 2} 3
                . update-x = r -> new_x -> {x = new_x, ...r}
                """
                    |> Expect.equal (Ok (Record (Dict.fromList [ ( "x", Int 3 ), ( "y", Int 2 ) ])))
        ]
