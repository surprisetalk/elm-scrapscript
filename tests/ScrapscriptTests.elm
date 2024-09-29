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


desc : String -> List Test -> Test
desc x =
    describe x << List.reverse


expectEqual : Result String value -> Result String value -> Expectation
expectEqual a_ b_ =
    case ( a_, b_ ) of
        ( Err a, Err b ) ->
            if String.contains a b then
                Expect.pass

            else
                Expect.fail (a ++ "\n\n" ++ b)

        ( a, b ) ->
            Expect.equal a b


suite : Test
suite =
    -- TODO: Fill in all the `Err ""` results to avoid false negatives.
    -- Tests initially cribbed from https://github.com/tekknolagi/scrapscript
    desc "Scrapscript"
        [ desc "Lexing" <|
            List.map (\( k, v ) -> test k <| \_ -> expectEqual v (parse k))
                [ ( "1", Ok (Int 1) )
                , ( "123", Ok (Int 123) )
                , ( "-123", Ok (Int -123) )
                , ( "3.14", Ok (Float 3.14) )
                , ( "-3.14", Ok (Float -3.14) )
                , ( ".14", Err "Parse error" )
                , ( "10.", Err "Parse error" )
                , ( "1 + 2", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( "1+2", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( ",:", Err "Parse error" )
                , ( "1-2", Ok (Binop "-" (Int 1) (Int 2)) )
                , ( "abc", Ok (Var "abc") )
                , ( "sha1'abc", Err "Parse error" )
                , ( "$sha1'foo", Ok (Var "$sha1'foo") )
                , ( "$$bills", Ok (Var "$$bills") )
                , ( "...", Ok (Spread Nothing) )
                , ( "1\n+\n2", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( "\"hello\"", Ok (Text "hello") )
                , ( "\"hello world\"", Ok (Text "hello world") )
                , ( "[ ]", Ok (List []) )
                , ( "[ 1 , 2 ]", Ok (List [ Int 1, Int 2 ]) )
                , ( "[1,2]", Ok (List [ Int 1, Int 2 ]) )
                , ( "a -> b -> a + b", Ok (Binop "->" (Var "a") (Binop "->" (Var "b") (Binop "+" (Var "a") (Var "b")))) )
                , ( "a->b->a+b", Ok (Binop "->" (Var "a") (Binop "->" (Var "b") (Binop "+" (Var "a") (Var "b")))) )
                , ( "a . b", Ok (Binop "." (Var "a") (Var "b")) )
                , ( "a ? b", Ok (Binop "?" (Var "a") (Var "b")) )
                , ( "a : b", Ok (Binop ":" (Var "a") (Var "b")) )
                , ( "-", Err "Parse error" )
                , ( "~~", Ok (Bytes "") )
                , ( "~~QUJD", Ok (Bytes "ABC") )
                , ( "~~85'K|(_", Ok (Bytes "ABC") )
                , ( "~~64'QUJD", Ok (Bytes "ABC") )
                , ( "~~32'IFBEG===", Ok (Bytes "ABC") )
                , ( "~~16'414243", Ok (Bytes "ABC") )
                , ( "()", Ok Hole )
                , ( "( )", Ok Hole )
                , ( "(1+2)", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( "1 |> f . f = a -> a + 1", Ok (Binop "." (Binop "|>" (Int 1) (Var "f")) (Binop "=" (Var "f") (Binop "->" (Var "a") (Binop "+" (Var "a") (Int 1))))) )
                , ( "f <| 1 . f = a -> a + 1", Ok (Binop "." (Binop "<|" (Var "f") (Int 1)) (Binop "=" (Var "f") (Binop "->" (Var "a") (Binop "+" (Var "a") (Int 1))))) )
                , ( "{ }", Ok (Record Dict.empty) )
                , ( "{}", Ok (Record Dict.empty) )
                , ( "{ a = 4 }", Ok (Record (Dict.fromList [ ( "a", Int 4 ) ])) )
                , ( "{ a = 4, b = \"z\" }", Ok (Record (Dict.fromList [ ( "a", Int 4 ), ( "b", Text "z" ) ])) )
                , ( "r@a", Ok (Binop "@" (Var "r") (Var "a")) )
                , ( "a!b", Ok (Binop "!" (Var "a") (Var "b")) )
                , ( "(| 1 -> 2)", Ok (Match [ Binop "->" (Int 1) (Int 2) ]) )
                , ( "| 1 -> 2", Ok (Match [ Binop "->" (Int 1) (Int 2) ]) )
                , ( "g = | 1 -> 2", Ok (Binop "=" (Var "g") (Match [ Binop "->" (Int 1) (Int 2) ])) )
                , ( "g = | 1 -> 2 | 2 -> 3", Ok (Binop "=" (Var "g") (Match [ Binop "->" (Int 1) (Int 2), Binop "->" (Int 2) (Int 3) ])) )
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
        , desc "Parsing" <|
            List.map (\( k, v ) -> test k <| always <| expectEqual v <| parse k)
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
                , ( "abc-123", Ok (Var "abc-123") )
                , ( "$sha1'abc", Ok (Var "$sha1'abc") )
                , ( "$sha1abc", Ok (Var "$sha1abc") )
                , ( "$", Ok (Var "$") )
                , ( "$$", Ok (Var "$$") )
                , ( "sha1'abc", Err "unexpected token" )
                , ( "$$bills", Ok (Var "$$bills") )
                , ( "~~QUJD", Ok (Bytes "ABC") )
                , ( "1 + 2", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( "1 - 2", Ok (Binop "-" (Int 1) (Int 2)) )
                , ( "1 + 2 + 3", Ok (Binop "+" (Binop "+" (Int 1) (Int 2)) (Int 3)) )
                , ( "1 + 2 * 3", Ok (Binop "+" (Int 1) (Binop "*" (Int 2) (Int 3))) )
                , ( "1 * 2 + 3", Ok (Binop "+" (Binop "*" (Int 1) (Int 2)) (Int 3)) )
                , ( "1 / 2 * 3", Ok (Binop "*" (Binop "/" (Int 1) (Int 2)) (Int 3)) )
                , ( "5 * 2 ^ 3", Ok (Binop "*" (Int 5) (Binop "^" (Int 2) (Int 3))) )
                , ( "a +< ls @ 0", Ok (Binop "+<" (Var "a") (Binop "@" (Var "ls") (Int 0))) )
                , ( "\"abc\" ++ \"def\"", Err "not allowed" )
                , ( "a >+ b", Ok (Binop ">+" (Var "a") (Var "b")) )
                , ( "a +< b", Ok (Binop "+<" (Var "a") (Var "b")) )
                , ( "[]", Ok (List []) )
                , ( "[1, 2]", Ok (List [ Int 1, Int 2 ]) )
                , ( "[,]", Err "Parse error" )
                , ( "[,,]", Err "Parse error" )
                , ( "[1,]", Err "Parse error" )
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
                , ( "{a = 4, b = \"z\"}", Ok (record [ ( "a", Int 4 ), ( "b", Text "z" ) ]) )
                , ( "3 = 4", Ok (Binop "=" (Int 3) (Int 4)) )
                , ( "{1, 2}", Err "Parse error" )
                , ( "a ! b", Ok (Binop "!" (Var "a") (Var "b")) )
                , ( "a ! b . c", Ok (Binop "!" (Var "a") (Binop "." (Var "b") (Var "c"))) )
                , ( "|", Err "Parse error" )
                , ( "| 1 -> 2", Err "TODO" )
                , ( "| 1 -> 2 | 2 -> 3", Err "TODO" )
                , ( "f >> g", Ok (Binop "->" (Var "$v0") (Apply (Var "g") (Apply (Var "f") (Var "$v0")))) )
                , ( "f << g", Ok (Binop "->" (Var "$v0") (Apply (Var "f") (Apply (Var "g") (Var "$v0")))) )
                , ( "f << g << h", Ok (Binop "->" (Var "$v1") (Apply (Var "f") (Apply (Binop "->" (Var "$v0") (Apply (Var "g") (Apply (Var "h") (Var "$v0")))) (Var "$v1")))) )
                , ( "x || y && z", Ok (Binop "||" (Var "x") (Binop "&&" (Var "y") (Var "z"))) )
                , ( "[1, ...]", Ok (List [ Int 1, Spread Nothing ]) )
                , ( "[1, ..., 2]", Err "TODO" )
                , ( "[1, ...rest]", Ok (List [ Int 1, Spread (Just (Var "rest")) ]) )
                , ( "[..., 1]", Err "TODO" )
                , ( "[...rest, 1]", Err "TODO" )
                , ( "[1, ..., 1]", Err "TODO" )
                , ( "[1, ...rest, 1]", Err "TODO" )
                , ( "{x = 1, ...}", Ok (record [ ( "x", Int 1 ), ( "...", Spread Nothing ) ]) )
                , ( "{..., x = 1}", Err "TODO" )
                , ( "{x = 1, ..., y = 2}", Err "TODO" )
                , ( "{,}", Err "Parse error" )
                , ( "{,,}", Err "Parse error" )
                , ( "{x = 1,}", Err "Parse error" )
                , ( "#abc 1", Ok (Variant "abc" (Int 1)) )
                , ( "| #true -> 123", Err "TODO" )
                , ( "| #true () -> 123", Err "TODO" )
                , ( "#true () && #false ()", Ok (Binop "&&" true false) )
                , ( "f #true () #false ()", Ok (Apply (Apply (Var "f") true) false) )
                ]
        , desc "Eval" <|
            List.map (\( env, k, v ) -> test k <| always <| expectEqual v <| run env k)
                [ ( Dict.empty, "5", Ok (Int 5) )
                , ( Dict.empty, "3.14", Ok (Float 3.14) )
                , ( Dict.empty, "\"xyz\"", Ok (Text "xyz") )
                , ( Dict.empty, "~~xyz", Ok (Bytes "xyz") )
                , ( Dict.empty, "no", Err "TODO" )
                , ( Dict.fromList [ ( "yes", Int 123 ) ], "yes", Ok (Int 123) )
                , ( Dict.empty, "1 + 2", Ok (Int 3) )
                , ( Dict.empty, "(1 + 2) + 3", Ok (Int 6) )
                , ( Dict.empty, "1 + \"hello\"", Err "" )
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
                , ( Dict.empty, "\"hello\" ++ \" world\"", Ok (Text "hello world") )
                , ( Dict.empty, "123 ++ \" world\"", Err "" )
                , ( Dict.empty, "\" world\" ++ 123", Err "" )
                , ( Dict.empty, "1 >+ [2, 3]", Ok (List [ Int 1, Int 2, Int 3 ]) )
                , ( Dict.empty, "[] >+ []", Ok (List [ List [] ]) )
                , ( Dict.empty, "[] >+ 123", Err "" )
                , ( Dict.empty, "[1, 2] +< 3", Ok (List [ Int 1, Int 2, Int 3 ]) )
                , ( Dict.empty, "[1 + 2, 3 + 4]", Ok (List [ Int 3, Int 7 ]) )
                , ( Dict.fromList [ ( "a", Int 1 ), ( "b", Int 2 ) ], "x -> x", Err "TODO" )
                , ( Dict.fromList [ ( "a", Int 1 ), ( "b", Int 2 ) ], "| ", Err "TODO" )
                , ( Dict.empty, "a = 1", Err "TODO" )
                , ( Dict.empty, "(a + 2) . a = 1", Ok (Int 3) )
                , ( Dict.empty, "((a + b) . a = 1) . b = 2", Ok (Int 3) )
                , ( Dict.empty, "123 ? #true ()", Ok (Int 123) )
                , ( Dict.empty, "123 ? #false ()", Err "" )
                , ( Dict.empty, "(123 ? #true ()) ? #true ()", Ok (Int 123) )
                , ( Dict.empty, "()", Ok Hole )
                , ( Dict.empty, "(x -> x + 1) 2", Ok (Int 3) )
                , ( Dict.empty, "((a -> b -> a + b) 3) 2", Ok (Int 5) )
                , ( Dict.fromList [ ( "y", Int 5 ) ], "(x -> x + y) 2", Ok (Int 7) )
                , ( Dict.empty, "3 4", Err "" )
                , ( Dict.empty, "4 @ \"x\"", Err "" )
                , ( Dict.empty, "{a = 1 + 2}", Ok (record [ ( "a", Int 3 ) ]) )
                , ( Dict.empty, "{a = 4} @ 0", Err "" )
                , ( Dict.empty, "{a = 4} @ b", Err "" )
                , ( Dict.empty, "{a = 4} @ a", Ok (Int 4) )
                , ( Dict.empty, "[4] @ \"hello\"", Err "" )
                , ( Dict.empty, "[1, 2, 3] @ 4", Err "" )
                , ( Dict.empty, "[\"a\", \"b\", \"c\"] @ 2", Ok (Text "c") )
                , ( Dict.empty, "1 ! 2", Ok (Int 2) )
                , ( Dict.empty, "(|) 1", Err "" )
                , ( Dict.empty, "(| 1 -> 2) 1", Ok (Int 2) )
                , ( Dict.empty, "(| 1 -> 2) 3", Err "" )
                , ( Dict.empty, "(| \"a\" -> \"b\") \"a\"", Ok (Text "b") )
                , ( Dict.empty, "(| \"a\" -> \"b\") \"c\"", Err "" )
                , ( Dict.empty, "(| 3 -> 4 | 1 -> 2) 1", Ok (Int 2) )
                , ( Dict.empty, "$$quote (1 + 2)", Ok (Binop "+" (Int 1) (Int 2)) )
                , ( Dict.empty, "((x = 1) . | y -> x) 2", Ok (Int 1) )
                , ( Dict.empty, "3 < 4", Ok true )
                , ( Dict.empty, "\"xyz\" < 4", Err "" )
                , ( Dict.empty, "3 <= 4", Ok true )
                , ( Dict.empty, "\"xyz\" <= 4", Err "" )
                , ( Dict.empty, "3 > 4", Ok false )
                , ( Dict.empty, "\"xyz\" > 4", Err "" )
                , ( Dict.empty, "3 >= 4", Ok false )
                , ( Dict.empty, "\"xyz\" >= 4", Err "" )
                , ( Dict.fromList [ ( "a", false ) ], "#true () && a", Ok false )
                , ( Dict.fromList [ ( "a", true ) ], "a && #false ()", Ok false )
                , ( Dict.fromList [ ( "a", true ) ], "#false () || a", Ok true )
                , ( Dict.fromList [ ( "a", false ) ], "a || #true ()", Ok true )
                , ( Dict.empty, "1 && 2", Err "" )
                , ( Dict.empty, "1 || 2", Err "" )
                , ( Dict.empty, "{x = ...}", Err "" )
                , ( Dict.empty, "#abc (1 + 2)", Ok (Variant "abc" (Int 3)) )
                , ( Dict.empty, "1.0 + 2.0", Ok (Float 3.0) )
                , ( Dict.empty, "1 + 2.0", Ok (Float 3.0) )
                , ( Dict.empty, "1 / 2.0", Ok (Float 0.5) )
                , ( Dict.empty, "1.0 / 2", Ok (Float 0.5) )
                , ( Dict.empty, "1 / 2", Ok (Float 0.5) )
                ]
        , desc "End-to-End" <|
            List.map (\( env, k, v ) -> test k <| always <| expectEqual v <| run env k)
                [ ( Dict.empty, "1", Ok (Int 1) )
                , ( Dict.empty, "3.14", Ok (Float 3.14) )
                , ( Dict.empty, "~~QUJD", Ok (Bytes "QUJD") )
                , ( Dict.empty, "1 + 2", Ok (Int 3) )
                , ( Dict.empty, "1 - 2", Ok (Int -1) )
                , ( Dict.empty, "\"abc\" ++ \"def\"", Ok (Text "abcdef") )
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
                , ( Dict.empty, "rec@b . rec = { a = 1, b = \"x\" }", Ok (Text "x") )
                , ( Dict.empty, "xs@1 . xs = [1, 2, 3]", Ok (Int 2) )
                , ( Dict.empty, "xs@y . y = 2 . xs = [1, 2, 3]", Ok (Int 3) )
                , ( Dict.empty, "xs@(1+1) . xs = [1, 2, 3]", Ok (Int 3) )
                , ( Dict.empty, "list-at 1 [1,2,3] . list-at = idx -> ls -> ls@idx", Ok (Int 2) )
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
                    get-x rec
                    . rec = { x = 3 }
                    . get-x =
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
                    get-x 3
                    . get-x =
                      | { x = x } -> x
                    """
                  , Err "TODO"
                  )
                , ( Dict.empty
                  , """
                    get-x rec
                    . rec = { a = 3, b = 3 }
                    . get-x =
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
                    get-x 3
                    . get-x =
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
                  , Ok (Text "omg")
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
                    update-x {x = 1, y = 2} 3
                    . update-x = r -> new-x -> {x = new-x, ...r}
                    """
                  , Ok (Record (Dict.fromList [ ( "x", Int 3 ), ( "y", Int 2 ) ]))
                  )
                ]
        ]
