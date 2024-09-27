module ScrapscriptTests exposing (suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Scrapscript exposing (Scrap(..), parse, run)
import Test exposing (..)


true : Scrap
true =
    Variant "true" Hole


false : Scrap
false =
    Variant "false" Hole


record : List ( String, Scrap ) -> Scrap
record =
    Dict.fromList >> Record


suite : Test
suite =
    describe "Scrapscript"
        [ describe "Lexing" <|
            List.map (\( k, v ) -> test k <| \_ -> Expect.equal v (parse k))
                [ ( "1", Ok (Int 1) )
                , ( "123", Ok (Int 123) )
                , ( "-123", Ok (Int -123) )
                , ( "3.14", Ok (Float 3.14) )
                , ( "-3.14", Ok (Float -3.14) )
                , ( ".14", Ok (Float 0.14) )
                , ( "10.", Ok (Float 10.0) )
                , ( "1 + 2", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( "1+2", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( ",:", Ok (Binop "," (Var "") (Var ":")) )
                , ( "1-2", Ok (Binop "-" (Int 1) (Int 2)) )
                , ( "abc", Ok (Var "abc") )
                , ( "sha1'abc", Ok (Var "sha1'abc") )
                , ( "$sha1'foo", Ok (Var "$sha1'foo") )
                , ( "$$bills", Ok (Var "$$bills") )
                , ( "...", Ok (Spread Nothing) )
                , ( "1\n+\t2", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( "\"hello\"", Ok (String "hello") )
                , ( "\"hello world\"", Ok (String "hello world") )
                , ( "[ ]", Ok (List []) )
                , ( "[ 1 , 2 ]", Ok (List [ Int 1, Int 2 ]) )
                , ( "[1,2]", Ok (List [ Int 1, Int 2 ]) )
                , ( "a -> b -> a + b", Ok (Binop "->" (Var "a") (Binop "->" (Var "b") (Binop "+" (Var "a") (Var "b")))) )
                , ( "a->b->a+b", Ok (Binop "->" (Var "a") (Binop "->" (Var "b") (Binop "+" (Var "a") (Var "b")))) )
                , ( "a . b", Ok (Binop "." (Var "a") (Var "b")) )
                , ( "a ? b", Ok (Binop "?" (Var "a") (Var "b")) )
                , ( "a : b", Ok (Binop ":" (Var "a") (Var "b")) )
                , ( "-", Ok (Var "-") )
                , ( "~~", Ok (Bytes "") )
                , ( "~~QUJD", Ok (Bytes "QUJD") )
                , ( "~~85'K|(_", Ok (Bytes "85'K|(_") )
                , ( "~~64'QUJD", Ok (Bytes "64'QUJD") )
                , ( "~~32'IFBEG===", Ok (Bytes "32'IFBEG===") )
                , ( "~~16'414243", Ok (Bytes "16'414243") )
                , ( "()", Ok Hole )
                , ( "( )", Ok Hole )
                , ( "(1+2)", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( "1 |> f . f = a -> a + 1", Ok (Binop "|>" (Int 1) (Binop "." (Var "f") (Binop "=" (Var "f") (Binop "->" (Var "a") (Binop "+" (Var "a") (Int 1)))))) )
                , ( "f <| 1 . f = a -> a + 1", Ok (Binop "<|" (Var "f") (Binop "." (Int 1) (Binop "=" (Var "f") (Binop "->" (Var "a") (Binop "+" (Var "a") (Int 1)))))) )
                , ( "{ }", Ok (Record Dict.empty) )
                , ( "{}", Ok (Record Dict.empty) )
                , ( "{ a = 4 }", Ok (Record (Dict.fromList [ ( "a", Int 4 ) ])) )
                , ( "{ a = 4, b = \"z\" }", Ok (Record (Dict.fromList [ ( "a", Int 4 ), ( "b", String "z" ) ])) )
                , ( "r@a", Ok (Binop "@" (Var "r") (Var "a")) )
                , ( "a!b", Ok (Binop "!" (Var "a") (Var "b")) )
                , ( "g = | 1 -> 2 | 2 -> 3", Ok (Binop "=" (Var "g") (Binop "|" (Binop "->" (Int 1) (Int 2)) (Binop "->" (Int 2) (Int 3)))) )
                , ( "f >> g", Ok (Binop ">>" (Var "f") (Var "g")) )
                , ( "f << g", Ok (Binop "<<" (Var "f") (Var "g")) )
                , ( "[ ... ]", Ok (List [ Spread Nothing ]) )
                , ( "[ 1 , ... ]", Ok (List [ Int 1, Spread Nothing ]) )
                , ( "[ 1,... ]", Ok (List [ Int 1, Spread Nothing ]) )
                , ( "[1,...rest]", Ok (List [ Int 1, Spread (Just (Var "rest")) ]) )
                , ( "{ ... }", Ok (Record (Dict.fromList [ ( "...", Spread Nothing ) ])) )
                , ( "{ x = 1, ...}", Ok (Record (Dict.fromList [ ( "x", Int 1 ), ( "...", Spread Nothing ) ])) )
                , ( "{x=1,...}", Ok (Record (Dict.fromList [ ( "x", Int 1 ), ( "...", Spread Nothing ) ])) )
                , ( "# abc", Ok (Variant "abc" Hole) )
                , ( "#abc", Ok (Variant "abc" Hole) )
                ]
        , describe "Parsing" <|
            List.map (\( k, v ) -> test k <| always <| Expect.equal v <| parse k)
                [ ( "", Err "unexpected end of input" )
                , ( "1", Ok (Int 1) )
                , ( "123", Ok (Int 123) )
                , ( "-123", Ok (Int -123) )
                , ( "-x", Ok (Binop "-" (Int 0) (Var "x")) )
                , ( "-l + r", Ok (Binop "+" (Binop "-" (Int 0) (Var "l")) (Var "r")) )
                , ( "-l * r", Ok (Binop "*" (Binop "-" (Int 0) (Var "l")) (Var "r")) )
                , ( "-l @ r", Ok (Binop "@" (Binop "-" (Int 0) (Var "l")) (Var "r")) )
                , ( "-l r", Ok (Apply (Binop "-" (Int 0) (Var "l")) (Var "r")) )
                , ( "3.14", Ok (Float 3.14) )
                , ( "-3.14", Ok (Binop "-" (Int 0) (Float 3.14)) )
                , ( "abc_123", Ok (Var "abc_123") )
                , ( "$sha1'abc", Ok (Var "$sha1'abc") )
                , ( "$sha1abc", Ok (Var "$sha1abc") )
                , ( "$", Ok (Var "$") )
                , ( "$$", Ok (Var "$$") )
                , ( "sha1'abc", Err "unexpected token" )
                , ( "$$bills", Ok (Var "$$bills") )
                , ( "~~QUJD", Ok (Bytes "ABC") )
                , ( "1 + 2", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( "1 - 2", Ok (Binop "-" (Int 1) (Int 2)) )
                , ( "1 + 2 + 3", Ok (Binop "+" (Int 1) (Binop "+" (Int 2) (Int 3))) )
                , ( "1 + 2 * 3", Ok (Binop "+" (Int 1) (Binop "*" (Int 2) (Int 3))) )
                , ( "1 * 2 + 3", Ok (Binop "+" (Binop "*" (Int 1) (Int 2)) (Int 3)) )
                , ( "1 / 3 * 3", Ok (Binop "*" (Binop "/" (Int 1) (Int 3)) (Int 3)) )
                , ( "5 * 2 ^ 3", Ok (Binop "*" (Int 5) (Binop "^" (Int 2) (Int 3))) )
                , ( "a +< ls @ 0", Ok (Binop "+<" (Var "a") (Binop "@" (Var "ls") (Int 0))) )
                , ( "\"abc\" ++ \"def\"", Ok (Binop "++" (String "abc") (String "def")) )
                , ( "a >+ b", Ok (Binop ">+" (Var "a") (Var "b")) )
                , ( "a +< b", Ok (Binop "+<" (Var "a") (Var "b")) )
                , ( "[]", Ok (List []) )
                , ( "[1, 2]", Ok (List [ Int 1, Int 2 ]) )
                , ( "[,]", Err "unexpected token Operator (lineno=-1, value=',')" )
                , ( "[,,]", Err "unexpected token Operator (lineno=-1, value=',')" )
                , ( "[1,]", Err "unexpected token RightBracket (lineno=-1)" )
                , ( "a = 1", Ok (Binop "=" (Var "a") (Int 1)) )
                , ( "a -> a + 1", Ok (Binop "->" (Var "a") (Binop "+" (Var "a") (Int 1))) )
                , ( "a -> b -> a + b", Ok (Binop "->" (Var "a") (Binop "->" (Var "b") (Binop "+" (Var "a") (Var "b")))) )
                , ( "id = x -> x", Ok (Binop "=" (Var "id") (Binop "->" (Var "x") (Var "x"))) )
                , ( "f a", Ok (Apply (Var "f") (Var "a")) )
                , ( "f a b", Ok (Apply (Apply (Var "f") (Var "a")) (Var "b")) )
                , ( "a . b", Ok (Binop "." (Var "a") (Var "b")) )
                , ( "a . b . c", Ok (Binop "." (Binop "." (Var "a") (Var "b")) (Var "c")) )
                , ( "a ? b", Ok (Binop "?" (Var "a") (Var "b")) )
                , ( "a ? b ? c", Ok (Binop "?" (Binop "?" (Var "a") (Var "b")) (Var "c")) )
                , ( "a ? b . c", Ok (Binop "." (Binop "?" (Var "a") (Var "b")) (Var "c")) )
                , ( "a : b", Ok (Binop ":" (Var "a") (Var "b")) )
                , ( "()", Ok Hole )
                , ( "(1 + 2)", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( "(1 + 2) * 3", Ok (Binop "*" (Binop "+" (Int 1) (Int 2)) (Int 3)) )
                , ( "1 |> f", Ok (Apply (Var "f") (Int 1)) )
                , ( "1 |> f |> g", Ok (Apply (Var "g") (Apply (Var "f") (Int 1))) )
                , ( "f <| 1", Ok (Apply (Var "f") (Int 1)) )
                , ( "g <| f <| 1", Ok (Apply (Var "g") (Apply (Var "f") (Int 1))) )
                , ( "{}", Ok (record []) )
                , ( "{a = 4}", Ok (record [ ( "a", Int 4 ) ]) )
                , ( "{a = 1 + 2}", Ok (record [ ( "a", Binop "+" (Int 1) (Int 2) ) ]) )
                , ( "{a = 4, b = \"z\"}", Ok (record [ ( "a", Int 4 ), ( "b", String "z" ) ]) )
                , ( "3 = 4", Err "expected variable in assignment Int (value=3)" )
                , ( "{1, 2}", Err "failed to parse variable assignment in record constructor" )
                , ( "a ! b", Ok (Binop "!" (Var "a") (Var "b")) )
                , ( "a ! b . c", Ok (Binop "!" (Var "a") (Binop "." (Var "b") (Var "c"))) )
                , ( "|", Err "unexpected end of input" )
                , ( "| 1 -> 2", Err "TODO" )
                , ( "| 1 -> 2 | 2 -> 3", Err "TODO" )
                , ( "f >> g", Ok (Binop "->" (Var "$v0") (Apply (Var "g") (Apply (Var "f") (Var "$v0")))) )
                , ( "f << g", Ok (Binop "->" (Var "$v0") (Apply (Var "f") (Apply (Var "g") (Var "$v0")))) )
                , ( "f << g << h", Ok (Binop "->" (Var "$v1") (Apply (Var "f") (Apply (Binop "->" (Var "$v0") (Apply (Var "g") (Apply (Var "h") (Var "$v0")))) (Var "$v1")))) )
                , ( "x || y && z", Ok (Binop "||" (Var "x") (Binop "&&" (Var "y") (Var "z"))) )
                , ( "[1, ...]", Ok (List [ Int 1, Spread Nothing ]) )
                , ( "[1, ..., 2]", Err "unexpected token IntLit (lineno=-1, value=1)" )
                , ( "[1, ...rest]", Ok (List [ Int 1, Spread (Just (Var "rest")) ]) )
                , ( "[..., 1]", Err "spread must come at end of list match" )
                , ( "[...rest, 1]", Err "spread must come at end of list match" )
                , ( "[1, ..., 1]", Err "spread must come at end of list match" )
                , ( "[1, ...rest, 1]", Err "spread must come at end of list match" )
                , ( "{x = 1, ...}", Ok (record [ ( "x", Int 1 ), ( "...", Spread Nothing ) ]) )
                , ( "{..., x = 1}", Err "spread must come at end of record match" )
                , ( "{x = 1, ..., y = 2}", Err "spread must come at end of record match" )
                , ( "{,}", Err "unexpected token Operator (lineno=-1, value=',')" )
                , ( "{,,}", Err "unexpected token Operator (lineno=-1, value=',')" )
                , ( "{x = 1,}", Err "unexpected token RightBrace (lineno=-1)" )
                , ( "#abc 1", Ok (Variant "abc" (Int 1)) )
                , ( "| #true () -> 123", Err "TODO" )
                , ( "#true () && #false ()", Ok (Binop "&&" true false) )
                , ( "f #true () #false ()", Ok (Apply (Apply (Var "f") true) false) )
                ]
        , describe "Eval" <|
            List.map (\( env, k, v ) -> test k <| always <| Expect.equal v <| run env k)
                [ ( Dict.empty, "5", Ok (Int 5) )
                , ( Dict.empty, "3.14", Ok (Float 3.14) )
                , ( Dict.empty, "\"xyz\"", Ok (String "xyz") )
                , ( Dict.empty, "~~xyz", Ok (Bytes "xyz") )
                , ( Dict.empty, "no", Err "TODO" )
                , ( Dict.fromList [ ( "yes", Int 123 ) ], "yes", Ok (Int 123) )
                , ( Dict.empty, "1 + 2", Ok (Int 3) )
                , ( Dict.empty, "(1 + 2) + 3", Ok (Int 6) )
                , ( Dict.empty, "1 + \"hello\"", Err "expected Int or Float, got String" )
                , ( Dict.empty, "1 - 2", Ok (Int -1) )
                , ( Dict.empty, "2 * 3", Ok (Int 6) )
                , ( Dict.empty, "3 / 10", Ok (Float 0.3) )
                , ( Dict.empty, "2 // 3", Ok (Int 0) )
                , ( Dict.empty, "2 ^ 3", Ok (Int 8) )
                , ( Dict.empty, "10 % 4", Ok (Int 2) )
                , ( Dict.empty, "1 == 1", Ok true )
                , ( Dict.empty, "1 == 2", Ok false )
                , ( Dict.empty, "1 != 1", Ok false )
                , ( Dict.empty, "1 != 2", Ok true )
                , ( Dict.empty, "\"hello\" ++ \" world\"", Ok (String "hello world") )
                , ( Dict.empty, "123 ++ \" world\"", Err "expected String, got Int" )
                , ( Dict.empty, "\" world\" ++ 123", Err "expected String, got Int" )
                , ( Dict.empty, "1 >+ [2, 3]", Ok (List [ Int 1, Int 2, Int 3 ]) )
                , ( Dict.empty, "[] >+ []", Ok (List [ List [] ]) )
                , ( Dict.empty, "[] >+ 123", Err "expected List, got Int" )
                , ( Dict.empty, "[1, 2] +< 3", Ok (List [ Int 1, Int 2, Int 3 ]) )
                , ( Dict.empty, "[1 + 2, 3 + 4]", Ok (List [ Int 3, Int 7 ]) )
                , ( Dict.fromList [ ( "a", Int 1 ), ( "b", Int 2 ) ], "x -> x", Err "TODO" )
                , ( Dict.fromList [ ( "a", Int 1 ), ( "b", Int 2 ) ], "| ", Err "TODO" )
                , ( Dict.empty, "a = 1", Err "TODO" )
                , ( Dict.empty, "(a + 2) . a = 1", Ok (Int 3) )
                , ( Dict.empty, "((a + b) . a = 1) . b = 2", Ok (Int 3) )
                , ( Dict.empty, "123 ? #true ()", Ok (Int 123) )
                , ( Dict.empty, "123 ? #false ()", Err "condition #false () failed" )
                , ( Dict.empty, "(123 ? #true ()) ? #true ()", Ok (Int 123) )
                , ( Dict.empty, "()", Ok Hole )
                , ( Dict.empty, "(x -> x + 1) 2", Ok (Int 3) )
                , ( Dict.empty, "((a -> b -> a + b) 3) 2", Ok (Int 5) )
                , ( Dict.fromList [ ( "y", Int 5 ) ], "(x -> x + y) 2", Ok (Int 7) )
                , ( Dict.empty, "3 4", Err "attempted to apply a non-closure of type Int" )
                , ( Dict.empty, "4 @ \"x\"", Err "attempted to access from type Int" )
                , ( Dict.empty, "{a = 1 + 2}", Ok (record [ ( "a", Int 3 ) ]) )
                , ( Dict.empty, "{a = 4} @ 0", Err "cannot access record field using Int, expected a field name" )
                , ( Dict.empty, "{a = 4} @ b", Err "no assignment to b found in record" )
                , ( Dict.empty, "{a = 4} @ a", Ok (Int 4) )
                , ( Dict.empty, "[4] @ \"hello\"", Err "cannot index into list using type String, expected integer" )
                , ( Dict.empty, "[1, 2, 3] @ 4", Err "index 4 out of bounds for list" )
                , ( Dict.empty, "[\"a\", \"b\", \"c\"] @ 2", Ok (String "c") )
                , ( Dict.empty, "1 ! 2", Ok (Int 2) )
                , ( Dict.empty, "(|) 1", Err "no matching cases" )
                , ( Dict.empty, "(| 1 -> 2) 1", Ok (Int 2) )
                , ( Dict.empty, "(| 1 -> 2) 3", Err "no matching cases" )
                , ( Dict.empty, "(| \"a\" -> \"b\") \"a\"", Ok (String "b") )
                , ( Dict.empty, "(| \"a\" -> \"b\") \"c\"", Err "no matching cases" )
                , ( Dict.empty, "(| 3 -> 4 | 1 -> 2) 1", Ok (Int 2) )
                , ( Dict.empty, "$$quote (1 + 2)", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( Dict.empty, "((x = 1) . | y -> x) 2", Ok (Int 1) )
                , ( Dict.empty, "3 < 4", Ok true )
                , ( Dict.empty, "\"xyz\" < 4", Err "expected Int or Float, got String" )
                , ( Dict.empty, "3 <= 4", Ok true )
                , ( Dict.empty, "\"xyz\" <= 4", Err "expected Int or Float, got String" )
                , ( Dict.empty, "3 > 4", Ok false )
                , ( Dict.empty, "\"xyz\" > 4", Err "expected Int or Float, got String" )
                , ( Dict.empty, "3 >= 4", Ok false )
                , ( Dict.empty, "\"xyz\" >= 4", Err "expected Int or Float, got String" )
                , ( Dict.fromList [ ( "a", false ) ], "#true () && a", Ok false )
                , ( Dict.fromList [ ( "a", true ) ], "a && #false ()", Ok false )
                , ( Dict.fromList [ ( "a", true ) ], "#false () || a", Ok true )
                , ( Dict.fromList [ ( "a", false ) ], "a || #true ()", Ok true )
                , ( Dict.empty, "1 && 2", Err "expected #true or #false, got Int" )
                , ( Dict.empty, "1 || 2", Err "expected #true or #false, got Int" )
                , ( Dict.empty, "{x = ...}", Err "cannot evaluate a spread" )
                , ( Dict.empty, "#abc (1 + 2)", Ok (Variant "abc" (Int 3)) )
                , ( Dict.empty, "1.0 + 2.0", Ok (Float 3.0) )
                , ( Dict.empty, "1 + 2.0", Ok (Float 3.0) )
                , ( Dict.empty, "1 / 2.0", Ok (Float 0.5) )
                , ( Dict.empty, "1.0 / 2", Ok (Float 0.5) )
                , ( Dict.empty, "1 / 2", Ok (Float 0.5) )
                ]
        , describe "End-to-End" <|
            List.map (\( env, k, v ) -> test k <| always <| Expect.equal v <| run env k)
                [ ( Dict.empty, "1", Ok (Int 1) )
                , ( Dict.empty, "3.14", Ok (Float 3.14) )
                , ( Dict.empty, "~~QUJD", Ok (Bytes "QUJD") )
                , ( Dict.empty, "1 + 2", Ok (Int 3) )
                , ( Dict.empty, "1 - 2", Ok (Int -1) )
                , ( Dict.empty, "\"abc\" ++ \"def\"", Ok (String "abcdef") )
                , ( Dict.empty, "1 >+ [2,3]", Ok (List [ Int 1, Int 2, Int 3 ]) )
                , ( Dict.empty, "1 >+ 2 >+ [3,4]", Ok (List [ Int 1, Int 2, Int 3, Int 4 ]) )
                , ( Dict.empty, "[1,2] +< 3", Ok (List [ Int 1, Int 2, Int 3 ]) )
                , ( Dict.empty, "[1,2] +< 3 +< 4", Ok (List [ Int 1, Int 2, Int 3, Int 4 ]) )
                , ( Dict.empty, "[ ]", Ok (List []) )
                , ( Dict.empty, "[]", Ok (List []) )
                , ( Dict.empty, "[ 1 , 2 ]", Ok (List [ Int 1, Int 2 ]) )
                , ( Dict.empty, "[ 1 + 2 , 3 + 4 ]", Ok (List [ Int 3, Int 7 ]) )
                , ( Dict.empty, "a + 2 . a = 1", Ok (Int 3) )
                , ( Dict.empty, "a + b . a = 1 . b = 2", Ok (Int 3) )
                , ( Dict.empty, "a + 1 ? a == 1 . a = 1", Ok (Int 2) )
                , ( Dict.empty, "a + 1 ? a == 2 . a = 1", Err "TODO" )
                , ( Dict.empty, "a + b ? a == 1 ? b == 2 . a = 1 . b = 2", Ok (Int 3) )
                , ( Dict.empty, "()", Ok Hole )
                , ( Dict.empty, "b . a = 1 . b = a", Err "TODO" )
                , ( Dict.empty, "(a -> b -> a + b) 3 2", Ok (Int 5) )
                , ( Dict.empty, "(a -> b -> [a, b]) 3 2", Ok (List [ Int 3, Int 2 ]) )
                , ( Dict.empty, "{a = 1 + 3}", Ok (Record (Dict.fromList [ ( "a", Int 4 ) ])) )
                , ( Dict.empty, "rec@b . rec = { a = 1, b = \"x\" }", Ok (String "x") )
                , ( Dict.empty, "xs@1 . xs = [1, 2, 3]", Ok (Int 2) )
                , ( Dict.empty, "xs@y . y = 2 . xs = [1, 2, 3]", Ok (Int 3) )
                , ( Dict.empty, "xs@(1+1) . xs = [1, 2, 3]", Ok (Int 3) )
                , ( Dict.empty, "list_at 1 [1,2,3] . list_at = idx -> ls -> ls@idx", Ok (Int 2) )
                , ( Dict.empty, "(x -> x) c . c = 1", Ok (Int 1) )
                , ( Dict.empty, "1 -> a", Err "TODO" )
                , ( Dict.empty, "((a -> a + 1) >> (b -> b * 2)) 3", Ok (Int 8) )
                , ( Dict.empty, "f 3 . f = ((y -> x) >> (z -> x))", Err "TODO" )
                , ( Dict.empty, "((a -> a + 1) >> (x -> x) >> (b -> b * 2)) 3", Ok (Int 8) )
                , ( Dict.empty, "((a -> a + 1) << (b -> b * 2)) 3", Ok (Int 7) )
                , ( Dict.empty, "(| [x, ...xs] -> xs) [1, 2]", Ok (List [ Int 2 ]) )
                , ( Dict.empty, "1 |> (a -> a + 2)", Ok (Int 3) )
                , ( Dict.empty, "1 |> (a -> a + 2) |> (b -> b * 2)", Ok (Int 6) )
                , ( Dict.empty, "(a -> a + 2) <| 1", Ok (Int 3) )
                , ( Dict.empty, "(b -> b * 2) <| (a -> a + 2) <| 1", Ok (Int 6) )
                , ( Dict.empty, "[1, 2, 3] +< xs@0 . xs = [4]", Ok (List [ Int 1, Int 2, Int 3, Int 4 ]) )
                , ( Dict.empty, "6 ^ 2", Ok (Int 36) )
                , ( Dict.empty, "11 % 3", Ok (Int 2) )
                , ( Dict.empty, "5 * 2 ^ 3", Ok (Int 40) )
                , ( Dict.empty, "# true ()", Ok (Variant "true" Hole) )
                , ( Dict.empty, "#false ()", Ok (Variant "false" Hole) )
                , ( Dict.empty, "#true () || #true () && boom", Ok (Variant "true" Hole) )
                , ( Dict.empty, "1 < 2 && 2 < 1", Ok (Variant "false" Hole) )
                , ( Dict.empty, "(| { a=1, ...rest } -> rest) {a=1, b=2, c=3}", Ok (Record (Dict.fromList [ ( "b", Int 2 ), ( "c", Int 3 ) ])) )
                , ( Dict.empty
                  , """
                inc 2
                . inc =
                  | 1 -> 2
                  | 2 -> 3
                """
                  , Ok (Int 3)
                  )
                , ( Dict.empty
                  , """
                id 3
                . id =
                  | x -> x
                """
                  , Ok (Int 3)
                  )
                , ( Dict.empty
                  , """
                id 3
                . id =
                  | x -> x
                  | y -> y * 2
                """
                  , Ok (Int 3)
                  )
                , ( Dict.empty
                  , """
                f 1 2
                . f = a ->
                  | b -> a + b
                """
                  , Ok (Int 3)
                  )
                , ( Dict.empty
                  , """
                get_x rec
                . rec = { x = 3 }
                . get_x =
                  | { x = x } -> x
                """
                  , Ok (Int 3)
                  )
                , ( Dict.empty
                  , """
                mult rec
                . rec = { x = 3, y = 4 }
                . mult =
                  | { x = x, y = y } -> x * y
                """
                  , Ok (Int 12)
                  )
                , ( Dict.empty
                  , """
                mult rec
                . rec = { x = 3 }
                . mult =
                  | { x = x, y = y } -> x * y
                """
                  , Err "TODO"
                  )
                , ( Dict.empty
                  , """
                mult rec
                . rec = { x = 4, y = 5 }
                . mult =
                  | { x = 3, y = y } -> 1
                  | { x = 4, y = y } -> 2
                """
                  , Ok (Int 2)
                  )
                , ( Dict.empty
                  , """
                get_x 3
                . get_x =
                  | { x = x } -> x
                """
                  , Err "TODO"
                  )
                , ( Dict.empty
                  , """
                get_x rec
                . rec = { a = 3, b = 3 }
                . get_x =
                  | { a = x, b = x } -> x
                """
                  , Ok (Int 3)
                  )
                , ( Dict.empty
                  , """
                mult xs
                . xs = [1, 2, 3, 4, 5]
                . mult =
                  | [1, x, 3, y, 5] -> x * y
                """
                  , Ok (Int 8)
                  )
                , ( Dict.empty
                  , """
                mult xs
                . xs = [1, 2, 3]
                . mult =
                  | [1, 2] -> 1
                  | [1, 2, 3, 4] -> 1
                  | [1, 3] -> 1
                """
                  , Err "TODO"
                  )
                , ( Dict.empty
                  , """
                middle xs
                . xs = [4, 5, 6]
                . middle =
                  | [1, x, 3] -> x
                  | [4, x, 6] -> x
                  | [7, x, 9] -> x
                """
                  , Ok (Int 5)
                  )
                , ( Dict.empty
                  , """
                get_x 3
                . get_x =
                  | [2, x] -> x
                """
                  , Err "TODO"
                  )
                , ( Dict.empty
                  , """
                mult xs
                . xs = [1, 2, 3, 2, 1]
                . mult =
                  | [1, x, 3, x, 1] -> x
                """
                  , Ok (Int 2)
                  )
                , ( Dict.empty
                  , """
                f 1
                . f = n -> f n
                """
                  , Err "TODO"
                  )
                , ( Dict.empty
                  , """
                fac 5
                . fac =
                  | 0 -> 1
                  | 1 -> 1
                  | n -> n * fac (n - 1)
                """
                  , Ok (Int 120)
                  )
                , ( Dict.empty
                  , """
                f [2, 4, 6]
                . f =
                  | [] -> 0
                  | [x, ...] -> x
                  | c -> 1
                """
                  , Ok (Int 2)
                  )
                , ( Dict.empty
                  , """
                tail [1,2,3]
                . tail =
                  | [first, ...rest] -> rest
                """
                  , Ok (List [ Int 2, Int 3 ])
                  )
                , ( Dict.empty
                  , """
                f {x = 4, y = 5}
                . f =
                  | {} -> 0
                  | {x = a, ...} -> a
                  | c -> 1
                """
                  , Ok (Int 4)
                  )
                , ( Dict.empty
                  , """
                say (1 < 2)
                . say =
                  | #false () -> "oh no"
                  | #true () -> "omg"
                """
                  , Ok (String "omg")
                  )
                , ( Dict.empty
                  , """
                f #add {x = 3, y = 4}
                . f =
                  | # b () -> "foo"
                  | #add {x = x, y = y} -> x + y
                """
                  , Ok (Int 7)
                  )
                , ( Dict.empty
                  , """
                x + y + z
                . x = 1
                . y = 2
                . z = 3
                """
                  , Ok (Int 6)
                  )
                , ( Dict.empty
                  , """
                outer 5
                . outer = x ->
                    inner (x + 1)
                    . inner = y -> y * 2
                """
                  , Ok (Int 12)
                  )
                , ( Dict.empty
                  , """
                apply (x -> x * 2) 3
                . apply = f -> x -> f x
                """
                  , Ok (Int 6)
                  )
                , ( Dict.empty
                  , """
                add 2 3
                . add = x -> y -> x + y
                """
                  , Ok (Int 5)
                  )
                , ( Dict.empty
                  , """
                map (x -> x * 2) [1, 2, 3]
                . map = f ->
                  | [] -> []
                  | [x, ...xs] -> f x >+ map f xs
                """
                  , Ok (List [ Int 2, Int 4, Int 6 ])
                  )
                , ( Dict.empty
                  , """
                update_x {x = 1, y = 2} 3
                . update-x = r -> new_x -> {x = new_x, ...r}
                """
                  , Ok (Record (Dict.fromList [ ( "x", Int 3 ), ( "y", Int 2 ) ]))
                  )

                {-
                    , (Dict.empty,
                     """
                            f 1
                            . f = n -> f
                            """
                   , (Ok (Closure (Dict.singleton "f" (Var "f")) (Binop "->" (Var "n") (Var "f"))))
                   )
                -}
                ]
        ]



{-
   suite : Test
   suite =
       describe "Scrapscript"
           [ describe "lex"
               [
               ]
           , describe "parse"
               [
               ]
           , describe "match"
               [ test "hole_with_non_hole_returns_none" <| \_ -> expectEqual ( match ( Int 1, Hole ), Err "" )
               , test "hole_with_hole_returns_empty_dict" <| \_ -> expectEqual ( match ( Hole, Hole ), Ok [] )
               , test "with_equal_ints_returns_empty_dict" <| \_ -> expectEqual ( match ( Int 1, Int 1 ), Ok [] )
               , test "with_inequal_ints_returns_none" <| \_ -> expectEqual ( match ( Int 2, Int 1 ), Err "" )
               , test "int_with_non_int_returns_none" <| \_ -> expectEqual ( match ( String "abc", Int 1 ), Err "" )
               , test "with_equal_floats_raises_match_error" <| \_ -> expectError "pattern matching is not supported for Floats" <| match ( Float 1, Float 1 )
               , test "with_inequal_floats_raises_match_error" <| \_ -> expectError "pattern matching is not supported for Floats" <| match ( Float 2, Float 1 )
               , test "float_with_non_float_raises_match_error" <| \_ -> expectError "pattern matching is not supported for Floats" <| match ( String "abc", Float 1 )
               , test "with_equal_strings_returns_empty_dict" <| \_ -> expectEqual ( match ( String "a", String "a" ), Ok [] )
               , test "with_inequal_strings_returns_none" <| \_ -> expectEqual ( match ( String "b", String "a" ), Err "" )
               , test "string_with_non_string_returns_none" <| \_ -> expectEqual ( match ( Int 1, String "abc" ), Err "" )
               , test "var_returns_dict_with_var_name" <| \_ -> expectEqual ( match ( String "abc", Var "a" ), Ok [ ( "a", String "abc" ) ] )
               , test "record_with_non_record_returns_none" <| \_ -> expectEqual ( match ( Int 2, Record [ ( "x", Var "x" ), ( "y", Var "y" ) ] ), Err "" )
               , test "record_with_more_fields_in_pattern_returns_none" <| \_ -> expectEqual ( match ( Record [ ( "x", Int 1 ), ( "y", Int 2 ) ], Record [ ( "x", Var "x" ), ( "y", Var "y" ), ( "z", Var "z" ) ] ), Err "" )
               , test "record_with_fewer_fields_in_pattern_returns_none" <| \_ -> expectEqual ( match ( Record [ ( "x", Int 1 ), ( "y", Int 2 ) ], Record [ ( "x", Var "x" ) ] ), Err "" )
               , test "record_with_vars_returns_dict_with_keys" <| \_ -> expectEqual ( match ( Record [ ( "x", Int 1 ), ( "y", Int 2 ) ], Record [ ( "x", Var "x" ), ( "y", Var "y" ) ] ), Ok [ ( "x", Int 1 ), ( "y", Int 2 ) ] )
               , test "record_with_matching_const_returns_dict_with_other_keys" <| \_ -> expectEqual ( match ( Record [ ( "x", Int 1 ), ( "y", Int 2 ) ], Record [ ( "x", Int 1 ), ( "y", Var "y" ) ] ), Ok [ ( "y", Int 2 ) ] )
               , test "record_with_non_matching_const_returns_none" <| \_ -> expectEqual ( match ( Record [ ( "x", Int 1 ), ( "y", Int 2 ) ], Record [ ( "x", Int 3 ), ( "y", Var "y" ) ] ), Err "" )
               , test "list_with_non_list_returns_none" <| \_ -> expectEqual ( match ( Int 2, List [ Var "x", Var "y" ] ), Err "" )
               , test "list_with_more_fields_in_pattern_returns_none" <| \_ -> expectEqual ( match ( List [ Int 1, Int 2 ], List [ Var "x", Var "y", Var "z" ] ), Err "" )
               , test "list_with_fewer_fields_in_pattern_returns_none" <| \_ -> expectEqual ( match ( List [ Int 1, Int 2 ], List [ Var "x" ] ), Err "" )
               , test "list_with_vars_returns_dict_with_keys" <| \_ -> expectEqual ( match ( List [ Int 1, Int 2 ], List [ Var "x", Var "y" ] ), Ok [ ( "x", Int 1 ), ( "y", Int 2 ) ] )
               , test "list_with_matching_const_returns_dict_with_other_keys" <| \_ -> expectEqual ( match ( List [ Int 1, Int 2 ], List [ Int 1, Var "y" ] ), Ok [ ( "y", Int 2 ) ] )
               , test "list_with_non_matching_const_returns_none" <| \_ -> expectEqual ( match ( List [ Int 1, Int 2 ], List [ Int 3, Var "y" ] ), Err "" )
               , test "right_pipe" <| \_ -> expectEqual ( Result.andThen parse (lex "3 + 4 |> $$quote"), Ok (Apply ( Var "$$quote", Binop ADD ( Int 3, Int 4 ) )) )
               , test "left_pipe" <| \_ -> expectEqual ( Result.andThen parse (lex "$$quote <| 3 + 4"), Ok (Apply ( Var "$$quote", Binop ADD ( Int 3, Int 4 ) )) )
               , test "with_left_apply" <| \_ -> expectTodo
               , test "with_right_apply" <| \_ -> expectTodo
               , test "list_with_spread_returns_empty_dict" <| \_ -> expectEqual ( match ( List [ Int 1, Int 2, Int 3, Int 4, Int 5 ], List [ Int 1, Spread Nothing ] ), Ok [] )
               , test "list_with_named_spread_returns_name_bound_to_rest" <| \_ -> expectEqual ( match ( List [ Int 1, Int 2, Int 3, Int 4 ], List [ Var "a", Int 2, Spread (Just (Var "rest")) ] ), Ok [ ( "a", Int 1 ), ( "rest", List [ Int 3, Int 4 ] ) ] )
               , test "list_with_named_spread_returns_name_bound_to_empty_rest" <| \_ -> expectEqual ( match ( List [ Int 1, Int 2 ], List [ Var "a", Int 2, Spread (Just (Var "rest")) ] ), Ok [ ( "a", Int 1 ), ( "rest", List [] ) ] )
               , test "list_with_mismatched_spread_returns_none" <| \_ -> expectEqual ( match ( List [ Int 1, Int 2, Int 3, Int 4, Int 5 ], List [ Int 1, Int 6, Spread Nothing ] ), Err "" )
               , test "record_with_constant_and_spread_returns_empty_dict" <| \_ -> expectEqual ( match ( Record [ ( "a", Int 1 ), ( "b", Int 2 ), ( "c", Int 3 ) ], Record [ ( "a", Int 1 ), ( "...", Spread Nothing ) ] ), Ok [] )
               , test "record_with_var_and_spread_returns_match" <| \_ -> expectEqual ( match ( Record [ ( "a", Int 1 ), ( "b", Int 2 ), ( "c", Int 3 ) ], Record [ ( "a", Var "x" ), ( "...", Spread Nothing ) ] ), Ok [ ( "x", Int 1 ) ] )
               , test "record_with_mismatched_spread_returns_none" <| \_ -> expectEqual ( match ( Record [ ( "a", Int 1 ), ( "b", Int 2 ), ( "c", Int 3 ) ], Record [ ( "d", Var "x" ), ( "...", Spread Nothing ) ] ), Err "" )
               , test "variant_with_equal_tag_returns_empty_dict" <| \_ -> expectEqual ( match ( Variant ( "abc", Hole ), Variant ( "abc", Hole ) ), Ok [] )
               , test "variant_with_inequal_tag_returns_none" <| \_ -> expectEqual ( match ( Variant ( "def", Hole ), Variant ( "abc", Hole ) ), Err "" )
               , test "variant_matches_value_1" <| \_ -> expectEqual ( match ( Variant ( "abc", Int 123 ), Variant ( "abc", Hole ) ), Err "" )
               , test "variant_matches_value_2" <| \_ -> expectEqual ( match ( Variant ( "abc", Int 123 ), Variant ( "abc", Int 123 ) ), Ok [] )
               , test "variant_with_different_type_returns_none" <| \_ -> expectEqual ( match ( Int 123, Variant ( "abc", Hole ) ), Err "" )
               ]
           , describe "eval"
               [
               ]
           , describe "EndToEndTests"
               [
               ]
           , describe "StdLibTests"
               [ test "add" <| \_ -> expectEqual ( run "$$add 3 4", Ok (Int 7) )
               , test "quote" <| \_ -> expectEqual ( run "$$quote (3 + 4)", Ok (Binop ADD ( Int 3, Int 4 )) )
               , test "quote_pipe" <| \_ -> expectEqual ( run "3 + 4 |> $$quote", Ok (Binop ADD ( Int 3, Int 4 )) )
               , test "quote_reverse_pipe" <| \_ -> expectEqual ( run "$$quote <| 3 + 4", Ok (Binop ADD ( Int 3, Int 4 )) )
               , test "serialize" <| \_ -> expectTodo
               , test "serialize_expr" <| \_ -> expectTodo
               , test "deserialize" <| \_ -> expectEqual ( run "$$deserialize ~~aQY=", Ok (Int 3) )
               , test "deserialize_expr" <| \_ -> expectEqual ( run "$$deserialize ~~KwIraQJpBA==", Ok (Binop ADD ( Int 1, Int 2 )) )
               , test "listlength_empty_list_returns_zero" <| \_ -> expectEqual ( run "$$listlength []", Ok (Int 0) )
               , test "listlength_returns_length" <| \_ -> expectEqual ( run "$$listlength [1,2,3]", Ok (Int 3) )
               , test "listlength_with_non_list_raises_type_error" <| \_ -> expectError "listlength expected List, but got Int" <| run "$$listlength 1"
               ]
           , describe "PreludeTests"
               [ test "id_returns_input" <| \_ -> expectEqual ( run "id 123", Ok (Int 123) )
               , test "filter_returns_matching" <| \_ -> expectEqual ( run """filter (x -> x < 4) [2, 6, 3, 7, 1, 8]""", Ok (List [ Int 2, Int 3, Int 1 ]) )
               , test "filter_with_function_returning_non_bool_raises_match_error" <| \_ -> expectError "" <| run """filter (x -> #no ()) [1]"""
               , test "quicksort" <| \_ -> expectEqual ( run """quicksort [2, 6, 3, 7, 1, 8]""", Ok (List [ Int 1, Int 2, Int 3, Int 6, Int 7, Int 8 ]) )
               , test "quicksort_with_empty_list" <| \_ -> expectEqual ( run """quicksort []""", Ok (List []) )
               , test "quicksort_with_non_int_raises_type_error" <| \_ -> expectError "" <| run """quicksort ["a", "c", "b"]"""
               , test "concat" <| \_ -> expectEqual ( run """concat [1, 2, 3] [4, 5, 6]""", Ok (List [ Int 1, Int 2, Int 3, Int 4, Int 5, Int 6 ]) )
               , test "concat_with_first_list_empty" <| \_ -> expectEqual ( run """concat [] [4, 5, 6]""", Ok (List [ Int 4, Int 5, Int 6 ]) )
               , test "concat_with_second_list_empty" <| \_ -> expectEqual ( run """concat [1, 2, 3] []""", Ok (List [ Int 1, Int 2, Int 3 ]) )
               , test "concat_with_both_lists_empty" <| \_ -> expectEqual ( run """concat [] []""", Ok (List []) )
               , test "map" <| \_ -> expectEqual ( run """map (x -> x * 2) [3, 1, 2]""", Ok (List [ Int 6, Int 2, Int 4 ]) )
               , test "map_with_non_function_raises_type_error" <| \_ -> expectError "" <| run """map 4 [3, 1, 2]"""
               , test "map_with_non_list_raises_match_error" <| \_ -> expectError "" <| run """map (x -> x * 2) 3"""
               , test "range" <| \_ -> expectEqual ( run """range 3""", Ok (List [ Int 0, Int 1, Int 2 ]) )
               , test "range_with_non_int_raises_type_error" <| \_ -> expectError "" <| run """range "a" """
               , test "foldr" <| \_ -> expectEqual ( run """foldr (x -> a -> a + x) 0 [1, 2, 3]""", Ok (Int 6) )
               , test "foldr_on_empty_list_returns_empty_list" <| \_ -> expectEqual ( run """foldr (x -> a -> a + x) 0 []""", Ok (Int 0) )
               , test "take" <| \_ -> expectEqual ( run """take 3 [1, 2, 3, 4, 5]""", Ok (List [ Int 1, Int 2, Int 3 ]) )
               , test "take_n_more_than_list_length_returns_full_list" <| \_ -> expectEqual ( run """take 5 [1, 2, 3]""", Ok (List [ Int 1, Int 2, Int 3 ]) )
               , test "take_with_non_int_raises_type_error" <| \_ -> expectError "" <| run """take "a" [1, 2, 3]"""
               , test "all_returns_true" <| \_ -> expectEqual ( run """all (x -> x < 5) [1, 2, 3, 4]""", Ok true )
               , test "all_returns_false" <| \_ -> expectEqual ( run """all (x -> x < 5) [2, 4, 6]""", Ok false )
               , test "all_with_empty_list_returns_true" <| \_ -> expectEqual ( run """all (x -> x == 5) []""", Ok true )
               , test "all_with_non_bool_raises_type_error" <| \_ -> expectError "" <| run """all (x -> x) [1, 2, 3]"""
               , test "all_short_circuits" <| \_ -> expectEqual ( run """all (x -> x > 1) [1, "a", "b"]""", Ok false )
               , test "any_returns_true" <| \_ -> expectEqual ( run """any (x -> x < 4) [1, 3, 5]""", Ok true )
               , test "any_returns_false" <| \_ -> expectEqual ( run """any (x -> x < 3) [4, 5, 6]""", Ok false )
               , test "any_with_empty_list_returns_false" <| \_ -> expectEqual ( run """any (x -> x == 5) []""", Ok false )
               , test "any_with_non_bool_raises_type_error" <| \_ -> expectError "" <| run """any (x -> x) [1, 2, 3]"""
               , test "any_short_circuits" <| \_ -> expectEqual ( run """any (x -> x > 1) [2, "a", "b"]""", Ok (Variant ( "true", Hole )) )
               , test "mul_and_div_have_left_to_right_precedence" <| \_ -> expectEqual ( run """1 / 3 * 3""", Ok (Float 1.0) )
               ]
           ]
-}
