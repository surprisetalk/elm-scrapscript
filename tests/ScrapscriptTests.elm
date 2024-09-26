module ScrapscriptTests exposing (suite)

import Dict exposing (Dict)
import Expect exposing (..)
import Scrapscript exposing (..)
import Test exposing (..)


expectError : String -> Result String a -> Expectation
expectError msg x =
    case x of
        Ok x_ ->
            Expect.fail ("Expected Err but received Ok: " ++ Debug.toString x_)

        Err err ->
            if String.contains msg err then
                Expect.pass

            else
                Expect.fail ("Expected `" ++ msg ++ "` but received `" ++ err ++ "`.")


expectEqual : ( a, a ) -> Expectation
expectEqual ( x, y ) =
    Expect.equal x y


expectTodo : Expectation
expectTodo =
    Expect.fail "TODO"


suite : Test
suite =
    describe "Scrapscript"
        [ describe "lex"
            [ test "digit" <| \_ -> Expect.equal (lex "1") (Ok [ IntLit 1 ])
            , test "multiple_digits" <| \_ -> Expect.equal (lex "123") (Ok [ IntLit 123 ])
            , test "negative_int" <| \_ -> Expect.equal (lex "-123") (Ok [ Operator "-", IntLit 123 ])
            , test "float" <| \_ -> Expect.equal (lex "3.14") (Ok [ FloatLit 3.14 ])
            , test "negative_float" <| \_ -> Expect.equal (lex "-3.14") (Ok [ Operator "-", FloatLit 3.14 ])
            , test "float_with_no_integer_part" <| \_ -> Expect.equal (lex ".14") (Ok [ FloatLit 0.14 ])
            , test "float_with_no_decimal_part" <| \_ -> Expect.equal (lex "10.") (Ok [ FloatLit 10.0 ])
            , test "float_with_multiple_decimal_points_raises_parse_error" <| \_ -> expectError "unexpected token '.'" (lex "1.0.1")
            , test "binop" <| \_ -> Expect.equal (lex "1 + 2") (Ok [ IntLit 1, Operator "+", IntLit 2 ])
            , test "binop_no_spaces" <| \_ -> Expect.equal (lex "1+2") (Ok [ IntLit 1, Operator "+", IntLit 2 ])
            , test "two_oper_chars_returns_two_ops" <| \_ -> Expect.equal (lex ",:") (Ok [ Operator ",", Operator ":" ])
            , test "binary_sub_no_spaces" <| \_ -> Expect.equal (lex "1-2") (Ok [ IntLit 1, Operator "-", IntLit 2 ])
            , test "var" <| \_ -> Expect.equal (lex "abc") (Ok [ Name "abc" ])
            , test "var_with_quote" <| \_ -> Expect.equal (lex "sha1'abc") (Ok [ Name "sha1'abc" ])
            , test "dollar_sha1_var" <| \_ -> Expect.equal (lex "$sha1'foo") (Ok [ Name "$sha1'foo" ])
            , test "dollar_dollar_var" <| \_ -> Expect.equal (lex "$$bills") (Ok [ Name "$$bills" ])
            , test "dot_dot_raises_parse_error" <| \_ -> expectError "unexpected token '..'" (lex "..")
            , test "spread" <| \_ -> Expect.equal (lex "...") (Ok [ Operator "..." ])
            , test "ignore_whitespace" <| \_ -> Expect.equal (lex "1\n+\t2") (Ok [ IntLit 1, Operator "+", IntLit 2 ])
            , test "ignore_line_comment" <| \_ -> expectTodo -- Expect.equal (lex "-- 1\n2") (Ok [ IntLit 2 ])
            , test "string" <| \_ -> Expect.equal (lex "\"hello\"") (Ok [ StringLit "hello" ])
            , test "string_with_spaces" <| \_ -> Expect.equal (lex "\"hello world\"") (Ok [ StringLit "hello world" ])
            , test "string_missing_end_quote_raises_parse_error" <| \_ -> expectError "ExpectingSymbol \"\\\"\"" <| lex "\"hello"
            , test "empty_list" <| \_ -> Expect.equal (lex "[ ]") (Ok [ LeftBracket, RightBracket ])
            , test "empty_list_with_spaces" <| \_ -> Expect.equal (lex "[ ]") (Ok [ LeftBracket, RightBracket ])
            , test "list_with_items" <| \_ -> Expect.equal (lex "[ 1 , 2 ]") (Ok [ LeftBracket, IntLit 1, Operator ",", IntLit 2, RightBracket ])
            , test "list_with_no_spaces" <| \_ -> Expect.equal (lex "[1,2]") (Ok [ LeftBracket, IntLit 1, Operator ",", IntLit 2, RightBracket ])
            , test "function" <| \_ -> Expect.equal (lex "a -> b -> a + b") (Ok [ Name "a", Operator "->", Name "b", Operator "->", Name "a", Operator "+", Name "b" ])
            , test "function_with_no_spaces" <| \_ -> Expect.equal (lex "a->b->a+b") (Ok [ Name "a", Operator "->", Name "b", Operator "->", Name "a", Operator "+", Name "b" ])
            , test "where" <| \_ -> Expect.equal (lex "a . b") (Ok [ Name "a", Operator ".", Name "b" ])
            , test "assert" <| \_ -> Expect.equal (lex "a ? b") (Ok [ Name "a", Operator "?", Name "b" ])
            , test "hastype" <| \_ -> Expect.equal (lex "a : b") (Ok [ Name "a", Operator ":", Name "b" ])
            , test "minus_returns_minus" <| \_ -> Expect.equal (lex "-") (Ok [ Operator "-" ])
            , test "tilde_raises_parse_error" <| \_ -> expectError "unexpected token '~'" <| lex "~"
            , test "tilde_equals_raises_parse_error" <| \_ -> expectError "unexpected token '~'" <| lex "~="
            , test "tilde_tilde_returns_empty_bytes" <| \_ -> Expect.equal (lex "~~") (Ok [ BytesLit ( "", 64 ) ])
            , test "bytes_returns_bytes_base64" <| \_ -> Expect.equal (lex "~~QUJD") (Ok [ BytesLit ( "QUJD", 64 ) ])
            , test "bytes_base85" <| \_ -> Expect.equal (lex "~~85'K|(_") (Ok [ BytesLit ( "K|(_", 85 ) ])
            , test "bytes_base64" <| \_ -> Expect.equal (lex "~~64'QUJD") (Ok [ BytesLit ( "QUJD", 64 ) ])
            , test "bytes_base32" <| \_ -> Expect.equal (lex "~~32'IFBEG===") (Ok [ BytesLit ( "IFBEG===", 32 ) ])
            , test "bytes_base16" <| \_ -> Expect.equal (lex "~~16'414243") (Ok [ BytesLit ( "414243", 16 ) ])
            , test "hole" <| \_ -> Expect.equal (lex "()") (Ok [ LeftParen, RightParen ])
            , test "hole_with_spaces" <| \_ -> Expect.equal (lex "( )") (Ok [ LeftParen, RightParen ])
            , test "parenthetical_expression" <| \_ -> Expect.equal (lex "(1+2)") (Ok [ LeftParen, IntLit 1, Operator "+", IntLit 2, RightParen ])
            , test "pipe" <| \_ -> Expect.equal (lex "1 |> f . f = a -> a + 1") (Ok [ IntLit 1, Operator "|>", Name "f", Operator ".", Name "f", Operator "=", Name "a", Operator "->", Name "a", Operator "+", IntLit 1 ])
            , test "reverse_pipe" <| \_ -> Expect.equal (lex "f <| 1 . f = a -> a + 1") (Ok [ Name "f", Operator "<|", IntLit 1, Operator ".", Name "f", Operator "=", Name "a", Operator "->", Name "a", Operator "+", IntLit 1 ])
            , test "record_no_fields" <| \_ -> Expect.equal (lex "{ }") (Ok [ LeftBrace, RightBrace ])
            , test "record_no_fields_no_spaces" <| \_ -> Expect.equal (lex "{}") (Ok [ LeftBrace, RightBrace ])
            , test "record_one_field" <| \_ -> Expect.equal (lex "{ a = 4 }") (Ok [ LeftBrace, Name "a", Operator "=", IntLit 4, RightBrace ])
            , test "record_multiple_fields" <| \_ -> Expect.equal (lex "{ a = 4, b = \"z\" }") (Ok [ LeftBrace, Name "a", Operator "=", IntLit 4, Operator ",", Name "b", Operator "=", StringLit "z", RightBrace ])
            , test "record_access" <| \_ -> Expect.equal (lex "r@a") (Ok [ Name "r", Operator "@", Name "a" ])
            , test "right_eval" <| \_ -> Expect.equal (lex "a!b") (Ok [ Name "a", Operator "!", Name "b" ])
            , test "match" <| \_ -> Expect.equal (lex "g = | 1 -> 2 | 2 -> 3") (Ok [ Name "g", Operator "=", Operator "|", IntLit 1, Operator "->", IntLit 2, Operator "|", IntLit 2, Operator "->", IntLit 3 ])
            , test "compose" <| \_ -> Expect.equal (lex "f >> g") (Ok [ Name "f", Operator ">>", Name "g" ])
            , test "compose_reverse" <| \_ -> Expect.equal (lex "f << g") (Ok [ Name "f", Operator "<<", Name "g" ])
            , test "list_with_only_spread" <| \_ -> Expect.equal (lex "[ ... ]") (Ok [ LeftBracket, Operator "...", RightBracket ])
            , test "list_with_spread" <| \_ -> Expect.equal (lex "[ 1 , ... ]") (Ok [ LeftBracket, IntLit 1, Operator ",", Operator "...", RightBracket ])
            , test "list_with_spread_no_spaces" <| \_ -> Expect.equal (lex "[ 1,... ]") (Ok [ LeftBracket, IntLit 1, Operator ",", Operator "...", RightBracket ])
            , test "list_with_named_spread" <| \_ -> Expect.equal (lex "[1,...rest]") (Ok [ LeftBracket, IntLit 1, Operator ",", Operator "...", Name "rest", RightBracket ])
            , test "record_with_only_spread" <| \_ -> Expect.equal (lex "{ ... }") (Ok [ LeftBrace, Operator "...", RightBrace ])
            , test "record_with_spread" <| \_ -> Expect.equal (lex "{ x = 1, ...}") (Ok [ LeftBrace, Name "x", Operator "=", IntLit 1, Operator ",", Operator "...", RightBrace ])
            , test "record_with_spread_no_spaces" <| \_ -> Expect.equal (lex "{x=1,...}") (Ok [ LeftBrace, Name "x", Operator "=", IntLit 1, Operator ",", Operator "...", RightBrace ])
            , test "variant_with_space" <| \_ -> Expect.equal (lex "# abc") (Ok [ VariantToken "abc" ])
            , test "variant_with_no_space" <| \_ -> Expect.equal (lex "#abc") (Ok [ VariantToken "abc" ])
            , test "variant_non_name_raises_parse_error" <| \_ -> expectError "expected name" <| lex "#1"
            , test "variant_eof_raises_unexpected_eof_error" <| \_ -> expectError "while reading symbol" <| lex "#"
            , test "binop_var" <| \_ -> expectTodo
            , test "with_trailing_whitespace" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal (lex "- ") (Ok [ Operator "-" ])
                        , \_ -> Expect.equal (lex "-- ") (Ok [])
                        , \_ -> Expect.equal (lex "+ ") (Ok [ Operator "+" ])
                        , \_ -> Expect.equal (lex "123 ") (Ok [ IntLit 123 ])
                        , \_ -> Expect.equal (lex "abc ") (Ok [ Name "abc" ])
                        , \_ -> Expect.equal (lex "[ ") (Ok [ LeftBracket ])
                        , \_ -> Expect.equal (lex "] ") (Ok [ RightBracket ])
                        ]
                        ()
            ]
        , describe "parse"
            [ test "with_empty_tokens_raises_parse_error" <| \_ -> Expect.equal (parse []) <| Err "unexpected end of input"
            , test "digit_returns_int" <| \_ -> Expect.equal (parse [ IntLit 1 ]) <| Ok (Int 1)
            , test "digits_returns_int" <| \_ -> Expect.equal (parse [ IntLit 123 ]) <| Ok (Int 123)
            , test "negative_int_returns_binary_sub_int" <| \_ -> Expect.equal (parse [ Operator "-", IntLit 123 ]) <| Ok (Binop SUB ( Int 0, Int 123 ))
            , test "negative_var_returns_binary_sub_int" <| \_ -> Expect.equal (parse [ Operator "-", Name "x" ]) <| Ok (Binop SUB ( Int 0, Var "x" ))
            , test "negative_int_binds_tighter_than_plus" <| \_ -> Expect.equal (parse [ Operator "-", Name "l", Operator "+", Name "r" ]) <| Ok (Binop ADD ( Binop SUB ( Int 0, Var "l" ), Var "r" ))
            , test "negative_int_binds_tighter_than_mul" <| \_ -> Expect.equal (parse [ Operator "-", Name "l", Operator "*", Name "r" ]) <| Ok (Binop MUL ( Binop SUB ( Int 0, Var "l" ), Var "r" ))
            , test "negative_int_binds_tighter_than_index" <| \_ -> Expect.equal (parse [ Operator "-", Name "l", Operator "@", Name "r" ]) <| Ok (Access ( Binop SUB ( Int 0, Var "l" ), Var "r" ))
            , test "negative_int_binds_tighter_than_apply" <| \_ -> Expect.equal (parse [ Operator "-", Name "l", Name "r" ]) <| Ok (Apply ( Binop SUB ( Int 0, Var "l" ), Var "r" ))
            , test "decimal_returns_float" <| \_ -> Expect.equal (parse [ FloatLit 3.14 ]) <| Ok (Float 3.14)
            , test "negative_float_returns_binary_sub_float" <| \_ -> Expect.equal (parse [ Operator "-", FloatLit 3.14 ]) <| Ok (Binop SUB ( Int 0, Float 3.14 ))
            , test "var_returns_var" <| \_ -> Expect.equal (parse [ Name "abc_123" ]) <| Ok (Var "abc_123")
            , test "sha_var_returns_var" <| \_ -> Expect.equal (parse [ Name "$sha1'abc" ]) <| Ok (Var "$sha1'abc")
            , test "sha_var_without_quote_returns_var" <| \_ -> Expect.equal (parse [ Name "$sha1abc" ]) <| Ok (Var "$sha1abc")
            , test "dollar_returns_var" <| \_ -> Expect.equal (parse [ Name "$" ]) <| Ok (Var "$")
            , test "dollar_dollar_returns_var" <| \_ -> Expect.equal (parse [ Name "$$" ]) <| Ok (Var "$$")
            , test "sha_var_without_dollar_raises_parse_error" <| \_ -> expectError "unexpected token" <| parse [ Name "sha1'abc" ]
            , test "dollar_dollar_var_returns_var" <| \_ -> Expect.equal (parse [ Name "$$bills" ]) <| Ok (Var "$$bills")
            , test "bytes_returns_bytes" <| \_ -> Expect.equal (parse [ BytesLit ( "QUJD", 64 ) ]) <| Ok (Bytes "ABC")
            , test "binary_add_returns_binop" <| \_ -> Expect.equal (parse [ IntLit 1, Operator "+", IntLit 2 ]) <| Ok (Binop ADD ( Int 1, Int 2 ))
            , test "binary_sub_returns_binop" <| \_ -> Expect.equal (parse [ IntLit 1, Operator "-", IntLit 2 ]) <| Ok (Binop SUB ( Int 1, Int 2 ))
            , test "binary_add_right_returns_binop" <| \_ -> Expect.equal (parse [ IntLit 1, Operator "+", IntLit 2, Operator "+", IntLit 3 ]) <| Ok (Binop ADD ( Int 1, Binop ADD ( Int 2, Int 3 ) ))
            , test "mul_binds_tighter_than_add_right" <| \_ -> Expect.equal (parse [ IntLit 1, Operator "+", IntLit 2, Operator "*", IntLit 3 ]) <| Ok (Binop ADD ( Int 1, Binop MUL ( Int 2, Int 3 ) ))
            , test "mul_binds_tighter_than_add_left" <| \_ -> Expect.equal (parse [ IntLit 1, Operator "*", IntLit 2, Operator "+", IntLit 3 ]) <| Ok (Binop ADD ( Binop MUL ( Int 1, Int 2 ), Int 3 ))
            , test "mul_and_div_bind_left_to_right" <| \_ -> Expect.equal (parse [ IntLit 1, Operator "/", IntLit 3, Operator "*", IntLit 3 ]) <| Ok (Binop MUL ( Binop DIV ( Int 1, Int 3 ), Int 3 ))
            , test "exp_binds_tighter_than_mul_right" <| \_ -> Expect.equal (parse [ IntLit 5, Operator "*", IntLit 2, Operator "^", IntLit 3 ]) <| Ok (Binop MUL ( Int 5, Binop EXP ( Int 2, Int 3 ) ))
            , test "list_access_binds_tighter_than_append" <| \_ -> Expect.equal (parse [ Name "a", Operator "+<", Name "ls", Operator "@", IntLit 0 ]) <| Ok (Binop LIST_APPEND ( Var "a", Access ( Var "ls", Int 0 ) ))
            , test "binary_str_concat_returns_binop" <| \_ -> Expect.equal (parse [ StringLit "abc", Operator "++", StringLit "def" ]) <| Ok (Binop STRING_CONCAT ( String "abc", String "def" ))
            , test "binary_list_cons_returns_binop" <| \_ -> Expect.equal (parse [ Name "a", Operator ">+", Name "b" ]) <| Ok (Binop LIST_CONS ( Var "a", Var "b" ))
            , test "binary_list_append_returns_binop" <| \_ -> Expect.equal (parse [ Name "a", Operator "+<", Name "b" ]) <| Ok (Binop LIST_APPEND ( Var "a", Var "b" ))
            , test "binary_op_returns_binop" <| \_ -> expectTodo
            , test "empty_list" <| \_ -> Expect.equal (parse [ LeftBracket, RightBracket ]) <| Ok (List [])
            , test "list_of_ints_returns_list" <| \_ -> Expect.equal (parse [ LeftBracket, IntLit 1, Operator ",", IntLit 2, RightBracket ]) <| Ok (List [ Int 1, Int 2 ])
            , test "list_with_only_comma_raises_parse_error" <| \_ -> expectError "unexpected token Operator (lineno=-1, value=',')" <| parse [ LeftBracket, Operator ",", RightBracket ]
            , test "list_with_two_commas_raises_parse_error" <| \_ -> expectError "unexpected token Operator (lineno=-1, value=',')" <| parse [ LeftBracket, Operator ",", Operator ",", RightBracket ]
            , test "list_with_trailing_comma_raises_parse_error" <| \_ -> expectError "unexpected token RightBracket (lineno=-1)" <| parse [ LeftBracket, IntLit 1, Operator ",", RightBracket ]
            , test "assign" <| \_ -> Expect.equal (parse [ Name "a", Operator "=", IntLit 1 ]) <| Ok (Assign ( Var "a", Int 1 ))
            , test "function_one_arg_returns_function" <| \_ -> Expect.equal (parse [ Name "a", Operator "->", Name "a", Operator "+", IntLit 1 ]) <| Ok (Function ( Var "a", Binop ADD ( Var "a", Int 1 ) ))
            , test "function_two_args_returns_functions" <| \_ -> Expect.equal (parse [ Name "a", Operator "->", Name "b", Operator "->", Name "a", Operator "+", Name "b" ]) <| Ok (Function ( Var "a", Function ( Var "b", Binop ADD ( Var "a", Var "b" ) ) ))
            , test "assign_function" <| \_ -> Expect.equal (parse [ Name "id", Operator "=", Name "x", Operator "->", Name "x" ]) <| Ok (Assign ( Var "id", Function ( Var "x", Var "x" ) ))
            , test "function_application_one_arg" <| \_ -> Expect.equal (parse [ Name "f", Name "a" ]) <| Ok (Apply ( Var "f", Var "a" ))
            , test "function_application_two_args" <| \_ -> Expect.equal (parse [ Name "f", Name "a", Name "b" ]) <| Ok (Apply ( Apply ( Var "f", Var "a" ), Var "b" ))
            , test "where" <| \_ -> Expect.equal (parse [ Name "a", Operator ".", Name "b" ]) <| Ok (Where ( Var "a", Var "b" ))
            , test "nested_where" <| \_ -> Expect.equal (parse [ Name "a", Operator ".", Name "b", Operator ".", Name "c" ]) <| Ok (Where ( Where ( Var "a", Var "b" ), Var "c" ))
            , test "assert" <| \_ -> Expect.equal (parse [ Name "a", Operator "?", Name "b" ]) <| Ok (Assert ( Var "a", Var "b" ))
            , test "nested_assert" <| \_ -> Expect.equal (parse [ Name "a", Operator "?", Name "b", Operator "?", Name "c" ]) <| Ok (Assert ( Assert ( Var "a", Var "b" ), Var "c" ))
            , test "mixed_assert_where" <| \_ -> Expect.equal (parse [ Name "a", Operator "?", Name "b", Operator ".", Name "c" ]) <| Ok (Where ( Assert ( Var "a", Var "b" ), Var "c" ))
            , test "hastype" <| \_ -> Expect.equal (parse [ Name "a", Operator ":", Name "b" ]) <| Ok (Binop HASTYPE ( Var "a", Var "b" ))
            , test "hole" <| \_ -> Expect.equal (parse [ LeftParen, RightParen ]) <| Ok Hole
            , test "parenthesized_expression" <| \_ -> Expect.equal (parse [ LeftParen, IntLit 1, Operator "+", IntLit 2, RightParen ]) <| Ok (Binop ADD ( Int 1, Int 2 ))
            , test "parenthesized_add_mul" <| \_ -> Expect.equal (parse [ LeftParen, IntLit 1, Operator "+", IntLit 2, RightParen, Operator "*", IntLit 3 ]) <| Ok (Binop MUL ( Binop ADD ( Int 1, Int 2 ), Int 3 ))
            , test "pipe" <| \_ -> Expect.equal (parse [ IntLit 1, Operator "|>", Name "f" ]) <| Ok (Apply ( Var "f", Int 1 ))
            , test "nested_pipe" <| \_ -> Expect.equal (parse [ IntLit 1, Operator "|>", Name "f", Operator "|>", Name "g" ]) <| Ok (Apply ( Var "g", Apply ( Var "f", Int 1 ) ))
            , test "reverse_pipe" <| \_ -> Expect.equal (parse [ Name "f", Operator "<|", IntLit 1 ]) <| Ok (Apply ( Var "f", Int 1 ))
            , test "nested_reverse_pipe" <| \_ -> Expect.equal (parse [ Name "g", Operator "<|", Name "f", Operator "<|", IntLit 1 ]) <| Ok (Apply ( Var "g", Apply ( Var "f", Int 1 ) ))
            , test "empty_record" <| \_ -> Expect.equal (parse [ LeftBrace, RightBrace ]) <| Ok (Record [])
            , test "record_single_field" <| \_ -> Expect.equal (parse [ LeftBrace, Name "a", Operator "=", IntLit 4, RightBrace ]) <| Ok (Record [ ( "a", Int 4 ) ])
            , test "record_with_expression" <| \_ -> Expect.equal (parse [ LeftBrace, Name "a", Operator "=", IntLit 1, Operator "+", IntLit 2, RightBrace ]) <| Ok (Record [ ( "a", Binop ADD ( Int 1, Int 2 ) ) ])
            , test "record_multiple_fields" <| \_ -> Expect.equal (parse [ LeftBrace, Name "a", Operator "=", IntLit 4, Operator ",", Name "b", Operator "=", StringLit "z", RightBrace ]) <| Ok (Record [ ( "a", Int 4 ), ( "b", String "z" ) ])
            , test "non_variable_in_assignment_raises_parse_error" <| \_ -> Expect.equal (parse [ IntLit 3, Operator "=", IntLit 4 ]) <| Err "expected variable in assignment Int (value=3)"
            , test "non_assign_in_record_constructor_raises_parse_error" <| \_ -> Expect.equal (parse [ LeftBrace, IntLit 1, Operator ",", IntLit 2, RightBrace ]) (Err "failed to parse variable assignment in record constructor")
            , test "right_eval_returns_binop" <| \_ -> Expect.equal (parse [ Name "a", Operator "!", Name "b" ]) <| Ok (Binop RIGHT_EVAL ( Var "a", Var "b" ))
            , test "right_eval_with_defs_returns_binop" <| \_ -> Expect.equal (parse [ Name "a", Operator "!", Name "b", Operator ".", Name "c" ]) <| Ok (Binop RIGHT_EVAL ( Var "a", Where ( Var "b", Var "c" ) ))
            , test "no_cases_raises_parse_error" <| \_ -> Expect.equal (parse [ Operator "|" ]) (Err "unexpected end of input")
            , test "one_case" <| \_ -> Expect.equal (parse [ Operator "|", IntLit 1, Operator "->", IntLit 2 ]) <| Ok (MatchFunction [ MatchCase ( Int 1, Int 2 ) ])
            , test "two_cases" <| \_ -> Expect.equal (parse [ Operator "|", IntLit 1, Operator "->", IntLit 2, Operator "|", IntLit 2, Operator "->", IntLit 3 ]) <| Ok (MatchFunction [ MatchCase ( Int 1, Int 2 ), MatchCase ( Int 2, Int 3 ) ])
            , test "compose" <| \_ -> Expect.equal (parse [ Name "f", Operator ">>", Name "g" ]) <| Ok (Function ( Var "$v0", Apply ( Var "g", Apply ( Var "f", Var "$v0" ) ) ))
            , test "compose_reverse" <| \_ -> Expect.equal (parse [ Name "f", Operator "<<", Name "g" ]) <| Ok (Function ( Var "$v0", Apply ( Var "f", Apply ( Var "g", Var "$v0" ) ) ))
            , test "double_compose" <| \_ -> Expect.equal (parse [ Name "f", Operator "<<", Name "g", Operator "<<", Name "h" ]) <| Ok (Function ( Var "$v1", Apply ( Var "f", Apply ( Function ( Var "$v0", Apply ( Var "g", Apply ( Var "h", Var "$v0" ) ) ), Var "$v1" ) ) ))
            , test "boolean_and_binds_tighter_than_or" <| \_ -> Expect.equal (parse [ Name "x", Operator "||", Name "y", Operator "&&", Name "z" ]) <| Ok (Binop BOOL_OR ( Var "x", Binop BOOL_AND ( Var "y", Var "z" ) ))
            , test "list_spread" <| \_ -> Expect.equal (parse [ LeftBracket, IntLit 1, Operator ",", Operator "...", RightBracket ]) <| Ok (List [ Int 1, Spread Nothing ])
            , test "list_with_non_name_expr_after_spread_raises_parse_error" <| \_ -> expectError "unexpected token IntLit (lineno=-1, value=1)" <| parse [ LeftBracket, IntLit 1, Operator ",", Operator "...", IntLit 2, RightBracket ]
            , test "list_with_named_spread" <| \_ -> Expect.equal (parse [ LeftBracket, IntLit 1, Operator ",", Operator "...", Name "rest", RightBracket ]) <| Ok (List [ Int 1, Spread (Just "rest") ])
            , test "list_spread_beginning_raises_parse_error" <| \_ -> expectError "spread must come at end of list match" <| parse [ LeftBracket, Operator "...", Operator ",", IntLit 1, RightBracket ]
            , test "list_named_spread_beginning_raises_parse_error" <| \_ -> expectError "spread must come at end of list match" <| parse [ LeftBracket, Operator "...", Name "rest", Operator ",", IntLit 1, RightBracket ]
            , test "list_spread_middle_raises_parse_error" <| \_ -> expectError "spread must come at end of list match" <| parse [ LeftBracket, IntLit 1, Operator ",", Operator "...", Operator ",", IntLit 1, RightBracket ]
            , test "list_named_spread_middle_raises_parse_error" <| \_ -> expectError "spread must come at end of list match" <| parse [ LeftBracket, IntLit 1, Operator ",", Operator "...", Name "rest", Operator ",", IntLit 1, RightBracket ]
            , test "record_spread" <| \_ -> Expect.equal (parse [ LeftBrace, Name "x", Operator "=", IntLit 1, Operator ",", Operator "...", RightBrace ]) <| Ok (Record [ ( "x", Int 1 ), ( "...", Spread Nothing ) ])
            , test "record_spread_beginning_raises_parse_error" <| \_ -> expectError "spread must come at end of record match" <| parse [ LeftBrace, Operator "...", Operator ",", Name "x", Operator "=", IntLit 1, RightBrace ]
            , test "record_spread_middle_raises_parse_error" <| \_ -> expectError "spread must come at end of record match" <| parse [ LeftBrace, Name "x", Operator "=", IntLit 1, Operator ",", Operator "...", Operator ",", Name "y", Operator "=", IntLit 2, RightBrace ]
            , test "record_with_only_comma_raises_parse_error" <| \_ -> expectError "unexpected token Operator (lineno=-1, value=',')" <| parse [ LeftBrace, Operator ",", RightBrace ]
            , test "record_with_two_commas_raises_parse_error" <| \_ -> expectError "unexpected token Operator (lineno=-1, value=',')" <| parse [ LeftBrace, Operator ",", Operator ",", RightBrace ]
            , test "record_with_trailing_comma_raises_parse_error" <| \_ -> expectError "unexpected token RightBrace (lineno=-1)" <| parse [ LeftBrace, Name "x", Operator "=", IntLit 1, Operator ",", RightBrace ]
            , test "variant_returns_variant" <| \_ -> Expect.equal (parse [ VariantToken "abc", IntLit 1 ]) <| Ok (Variant ( "abc", Int 1 ))
            , test "with_variant" <| \_ -> Expect.equal (Result.andThen parse (lex "| #true () -> 123")) <| Ok (MatchFunction [ MatchCase ( true, Int 123 ) ])
            , test "binary_and_with_variant_args" <| \_ -> Expect.equal (Result.andThen parse (lex "#true () && #false ()")) <| Ok (Binop BOOL_AND ( true, false ))
            , test "apply_with_variant_args" <| \_ -> Expect.equal (Result.andThen parse (lex "f #true () #false ()")) <| Ok (Apply ( Apply ( Var "f", true ), false ))
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
            , test "list_with_named_spread_returns_name_bound_to_rest" <| \_ -> expectEqual ( match ( List [ Int 1, Int 2, Int 3, Int 4 ], List [ Var "a", Int 2, Spread (Just "rest") ] ), Ok [ ( "a", Int 1 ), ( "rest", List [ Int 3, Int 4 ] ) ] )
            , test "list_with_named_spread_returns_name_bound_to_empty_rest" <| \_ -> expectEqual ( match ( List [ Int 1, Int 2 ], List [ Var "a", Int 2, Spread (Just "rest") ] ), Ok [ ( "a", Int 1 ), ( "rest", List [] ) ] )
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
            [ test "int_returns_int" <| \_ -> expectEqual ( eval ( Dict.empty, Int 5 ), Ok (Int 5) )
            , test "float_returns_float" <| \_ -> expectEqual ( eval ( Dict.empty, Float 3.14 ), Ok (Float 3.14) )
            , test "str_returns_str" <| \_ -> expectEqual ( eval ( Dict.empty, String "xyz" ), Ok (String "xyz") )
            , test "bytes_returns_bytes" <| \_ -> expectEqual ( eval ( Dict.empty, Bytes "xyz" ), Ok (Bytes "xyz") )
            , test "with_non_existent_var_raises_name_error" <| \_ -> expectError "name 'no' is not defined" <| eval ( Dict.empty, Var "no" )
            , test "with_bound_var_returns_value" <| \_ -> expectEqual ( eval ( Dict.fromList [ ( "yes", Int 123 ) ], Var "yes" ), Ok (Int 123) )
            , test "with_binop_add_returns_sum" <| \_ -> expectEqual ( eval ( Dict.empty, Binop ADD ( Int 1, Int 2 ) ), Ok (Int 3) )
            , test "with_nested_binop" <| \_ -> expectEqual ( eval ( Dict.empty, Binop ADD ( Binop ADD ( Int 1, Int 2 ), Int 3 ) ), Ok (Int 6) )
            , test "with_binop_add_with_int_string_raises_type_error" <| \_ -> expectError "expected Int or Float, got String" <| eval ( Dict.empty, Binop ADD ( Int 1, String "hello" ) )
            , test "with_binop_sub" <| \_ -> expectEqual ( eval ( Dict.empty, Binop SUB ( Int 1, Int 2 ) ), Ok (Int -1) )
            , test "with_binop_mul" <| \_ -> expectEqual ( eval ( Dict.empty, Binop MUL ( Int 2, Int 3 ) ), Ok (Int 6) )
            , test "with_binop_div" <| \_ -> expectEqual ( eval ( Dict.empty, Binop DIV ( Int 3, Int 10 ) ), Ok (Float 0.3) )
            , test "with_binop_floor_div" <| \_ -> expectEqual ( eval ( Dict.empty, Binop FLOOR_DIV ( Int 2, Int 3 ) ), Ok (Int 0) )
            , test "with_binop_exp" <| \_ -> expectEqual ( eval ( Dict.empty, Binop EXP ( Int 2, Int 3 ) ), Ok (Int 8) )
            , test "with_binop_mod" <| \_ -> expectEqual ( eval ( Dict.empty, Binop MOD ( Int 10, Int 4 ) ), Ok (Int 2) )
            , test "with_binop_equal_with_equal_returns_true" <| \_ -> expectEqual ( eval ( Dict.empty, Binop EQUAL ( Int 1, Int 1 ) ), Ok true )
            , test "with_binop_equal_with_inequal_returns_false" <| \_ -> expectEqual ( eval ( Dict.empty, Binop EQUAL ( Int 1, Int 2 ) ), Ok false )
            , test "with_binop_not_equal_with_equal_returns_false" <| \_ -> expectEqual ( eval ( Dict.empty, Binop NOT_EQUAL ( Int 1, Int 1 ) ), Ok false )
            , test "with_binop_not_equal_with_inequal_returns_true" <| \_ -> expectEqual ( eval ( Dict.empty, Binop NOT_EQUAL ( Int 1, Int 2 ) ), Ok true )
            , test "with_binop_concat_with_strings_returns_string" <| \_ -> expectEqual ( eval ( Dict.empty, Binop STRING_CONCAT ( String "hello", String " world" ) ), Ok (String "hello world") )
            , test "with_binop_concat_with_int_string_raises_type_error" <| \_ -> expectError "expected String, got Int" <| eval ( Dict.empty, Binop STRING_CONCAT ( Int 123, String " world" ) )
            , test "with_binop_concat_with_string_int_raises_type_error" <| \_ -> expectError "expected String, got Int" <| eval ( Dict.empty, Binop STRING_CONCAT ( String " world", Int 123 ) )
            , test "with_binop_cons_with_int_list_returns_list" <| \_ -> expectEqual ( eval ( Dict.empty, Binop LIST_CONS ( Int 1, List [ Int 2, Int 3 ] ) ), Ok (List [ Int 1, Int 2, Int 3 ]) )
            , test "with_binop_cons_with_list_list_returns_nested_list" <| \_ -> expectEqual ( eval ( Dict.empty, Binop LIST_CONS ( List [], List [] ) ), Ok (List [ List [] ]) )
            , test "with_binop_cons_with_list_int_raises_type_error" <| \_ -> expectError "expected List, got Int" <| eval ( Dict.empty, Binop LIST_CONS ( List [], Int 123 ) )
            , test "with_list_append" <| \_ -> expectEqual ( eval ( Dict.empty, Binop LIST_APPEND ( List [ Int 1, Int 2 ], Int 3 ) ), Ok (List [ Int 1, Int 2, Int 3 ]) )
            , test "with_list_evaluates_elements" <| \_ -> expectEqual ( eval ( Dict.empty, List [ Binop ADD ( Int 1, Int 2 ), Binop ADD ( Int 3, Int 4 ) ] ), Ok (List [ Int 3, Int 7 ]) )
            , test "with_function_returns_closure_with_improved_env" <| \_ -> expectEqual ( eval ( Dict.fromList [ ( "a", Int 1 ), ( "b", Int 2 ) ], Function ( Var "x", Var "x" ) ), Ok (Closure ( Dict.empty, Function ( Var "x", Var "x" ) )) )
            , test "with_match_function_returns_closure_with_improved_env" <| \_ -> expectEqual ( eval ( Dict.fromList [ ( "a", Int 1 ), ( "b", Int 2 ) ], MatchFunction [] ), Ok (Closure ( Dict.empty, MatchFunction [] )) )
            , test "assign_returns_env_object" <| \_ -> expectEqual ( eval ( Dict.empty, Assign ( Var "a", Int 1 ) ), Ok (EnvObject (Dict.fromList [ ( "a", Int 1 ) ])) )
            , test "assign_function_returns_closure_without_function_in_env" <| \_ -> expectTodo
            , test "assign_function_returns_closure_with_function_in_env" <| \_ -> expectTodo
            , test "where_evaluates_in_order" <| \_ -> expectEqual ( eval ( Dict.empty, Where ( Binop ADD ( Var "a", Int 2 ), Assign ( Var "a", Int 1 ) ) ), Ok (Int 3) )
            , test "nested_where" <| \_ -> expectEqual ( eval ( Dict.empty, Where ( Where ( Binop ADD ( Var "a", Var "b" ), Assign ( Var "a", Int 1 ) ), Assign ( Var "b", Int 2 ) ) ), Ok (Int 3) )
            , test "assert_with_truthy_cond_returns_value" <| \_ -> expectEqual ( eval ( Dict.empty, Assert ( Int 123, true ) ), Ok (Int 123) )
            , test "assert_with_falsey_cond_raises_assertion_error" <| \_ -> expectError "condition #false () failed" <| eval ( Dict.empty, Assert ( Int 123, false ) )
            , test "nested_assert" <| \_ -> expectEqual ( eval ( Dict.empty, Assert ( Assert ( Int 123, true ), true ) ), Ok (Int 123) )
            , test "hole" <| \_ -> expectEqual ( eval ( Dict.empty, Hole ), Ok Hole )
            , test "function_application_one_arg" <| \_ -> expectEqual ( eval ( Dict.empty, Apply ( Function ( Var "x", Binop ADD ( Var "x", Int 1 ) ), Int 2 ) ), Ok (Int 3) )
            , test "function_application_two_args" <| \_ -> expectEqual ( eval ( Dict.empty, Apply ( Apply ( Function ( Var "a", Function ( Var "b", Binop ADD ( Var "a", Var "b" ) ) ), Int 3 ), Int 2 ) ), Ok (Int 5) )
            , test "function_returns_closure_with_captured_env" <| \_ -> expectTodo
            , test "function_capture_env" <| \_ -> expectEqual ( eval ( Dict.fromList [ ( "y", Int 5 ) ], Apply ( Function ( Var "x", Binop ADD ( Var "x", Var "y" ) ), Int 2 ) ), Ok (Int 7) )
            , test "non_function_raises_type_error" <| \_ -> expectError "attempted to apply a non-closure of type Int" <| eval ( Dict.empty, Apply ( Int 3, Int 4 ) )
            , test "access_from_invalid_object_raises_type_error" <| \_ -> expectError "attempted to access from type Int" <| eval ( Dict.empty, Access ( Int 4, String "x" ) )
            , test "record_evaluates_value_expressions" <| \_ -> expectEqual ( eval ( Dict.empty, Record [ ( "a", Binop ADD ( Int 1, Int 2 ) ) ] ), Ok (Record [ ( "a", Int 3 ) ]) )
            , test "record_access_with_invalid_accessor_raises_type_error" <| \_ -> expectError "cannot access record field using Int, expected a field name" <| eval ( Dict.empty, Access ( Record [ ( "a", Int 4 ) ], Int 0 ) )
            , test "record_access_with_unknown_accessor_raises_name_error" <| \_ -> expectError "no assignment to b found in record" <| eval ( Dict.empty, Access ( Record [ ( "a", Int 4 ) ], Var "b" ) )
            , test "record_access" <| \_ -> expectEqual ( eval ( Dict.empty, Access ( Record [ ( "a", Int 4 ) ], Var "a" ) ), Ok (Int 4) )
            , test "list_access_with_invalid_accessor_raises_type_error" <| \_ -> expectError "cannot index into list using type String, expected integer" <| eval ( Dict.empty, Access ( List [ Int 4 ], String "hello" ) )
            , test "list_access_with_out_of_bounds_accessor_raises_value_error" <| \_ -> expectError "index 4 out of bounds for list" <| eval ( Dict.empty, Access ( List [ Int 1, Int 2, Int 3 ], Int 4 ) )
            , test "list_access" <| \_ -> expectEqual ( eval ( Dict.empty, Access ( List [ String "a", String "b", String "c" ], Int 2 ) ), Ok (String "c") )
            , test "right_eval_evaluates_right_hand_side" <| \_ -> expectEqual ( eval ( Dict.empty, Binop RIGHT_EVAL ( Int 1, Int 2 ) ), Ok (Int 2) )
            , test "no_cases_raises_match_error" <| \_ -> expectError "no matching cases" <| eval ( Dict.empty, Apply ( MatchFunction [], Int 1 ) )
            , test "int_with_equal_int_matches" <| \_ -> expectEqual ( eval ( Dict.empty, Apply ( MatchFunction [ MatchCase ( Int 1, Int 2 ) ], Int 1 ) ), Ok (Int 2) )
            , test "int_with_inequal_int_raises_match_error" <| \_ -> expectError "no matching cases" <| eval ( Dict.empty, Apply ( MatchFunction [ MatchCase ( Int 1, Int 2 ) ], Int 3 ) )
            , test "string_with_equal_string_matches" <| \_ -> expectEqual ( eval ( Dict.empty, Apply ( MatchFunction [ MatchCase ( String "a", String "b" ) ], String "a" ) ), Ok (String "b") )
            , test "string_with_inequal_string_raises_match_error" <| \_ -> expectError "no matching cases" <| eval ( Dict.empty, Apply ( MatchFunction [ MatchCase ( String "a", String "b" ) ], String "c" ) )
            , test "falls_through_to_next" <| \_ -> expectEqual ( eval ( Dict.empty, Apply ( MatchFunction [ MatchCase ( Int 3, Int 4 ), MatchCase ( Int 1, Int 2 ) ], Int 1 ) ), Ok (Int 2) )
            , test "compose" <| \_ -> expectEqual ( Result.andThen (eval << Tuple.pair (Dict.fromList [ ( "a", Int 1 ) ])) (Result.andThen parse (lex "(x -> x + 3) << (x -> x * 2)")), Ok (Closure ( Dict.empty, Function ( Var "$v0", Apply ( Function ( Var "x", Binop ADD ( Var "x", Int 3 ) ), Apply ( Function ( Var "x", Binop MUL ( Var "x", Int 2 ) ), Var "$v0" ) ) ) )) )
            , test "native_function_returns_function" <| \_ -> expectTodo
            , test "apply_native_function_calls_function" <| \_ -> expectTodo
            , test "apply_quote_returns_ast" <| \_ -> expectEqual ( eval ( Dict.empty, Apply ( Var "$$quote", Binop ADD ( Int 1, Int 2 ) ) ), Ok (Binop ADD ( Int 1, Int 2 )) )
            , test "apply_closure_with_match_function_has_access_to_closure_vars" <| \_ -> expectEqual ( eval ( Dict.empty, Apply ( Closure ( Dict.fromList [ ( "x", Int 1 ) ], MatchFunction [ MatchCase ( Var "y", Var "x" ) ] ), Int 2 ) ), Ok (Int 1) )
            , test "less_returns_bool" <| \_ -> expectEqual ( eval ( Dict.empty, Binop LESS ( Int 3, Int 4 ) ), Ok true )
            , test "less_on_non_bool_raises_type_error" <| \_ -> expectError "expected Int or Float, got String" <| eval ( Dict.empty, Binop LESS ( String "xyz", Int 4 ) )
            , test "less_equal_returns_bool" <| \_ -> expectEqual ( eval ( Dict.empty, Binop LESS_EQUAL ( Int 3, Int 4 ) ), Ok true )
            , test "less_equal_on_non_bool_raises_type_error" <| \_ -> expectError "expected Int or Float, got String" <| eval ( Dict.empty, Binop LESS_EQUAL ( String "xyz", Int 4 ) )
            , test "greater_returns_bool" <| \_ -> expectEqual ( eval ( Dict.empty, Binop GREATER ( Int 3, Int 4 ) ), Ok false )
            , test "greater_on_non_bool_raises_type_error" <| \_ -> expectError "expected Int or Float, got String" <| eval ( Dict.empty, Binop GREATER ( String "xyz", Int 4 ) )
            , test "greater_equal_returns_bool" <| \_ -> expectEqual ( eval ( Dict.empty, Binop GREATER_EQUAL ( Int 3, Int 4 ) ), Ok false )
            , test "greater_equal_on_non_bool_raises_type_error" <| \_ -> expectError "expected Int or Float, got String" <| eval ( Dict.empty, Binop GREATER_EQUAL ( String "xyz", Int 4 ) )
            , test "boolean_and_evaluates_args_1" <| \_ -> expectEqual ( eval ( Dict.fromList [ ( "a", false ) ], Binop BOOL_AND ( true, Var "a" ) ), Ok false )
            , test "boolean_and_evaluates_args_2" <| \_ -> expectEqual ( eval ( Dict.fromList [ ( "a", true ) ], Binop BOOL_AND ( Var "a", false ) ), Ok false )
            , test "boolean_or_evaluates_args_1" <| \_ -> expectEqual ( eval ( Dict.fromList [ ( "a", true ) ], Binop BOOL_OR ( false, Var "a" ) ), Ok true )
            , test "boolean_or_evaluates_args_2" <| \_ -> expectEqual ( eval ( Dict.fromList [ ( "a", false ) ], Binop BOOL_OR ( Var "a", true ) ), Ok true )
            , test "boolean_and_short_circuit" <| \_ -> expectTodo
            , test "boolean_or_short_circuit" <| \_ -> expectTodo
            , test "boolean_and_on_int_raises_type_error" <| \_ -> expectError "expected #true or #false, got Int" <| eval ( Dict.empty, Binop BOOL_AND ( Int 1, Int 2 ) )
            , test "boolean_or_on_int_raises_type_error" <| \_ -> expectError "expected #true or #false, got Int" <| eval ( Dict.empty, Binop BOOL_OR ( Int 1, Int 2 ) )
            , test "record_with_spread_fails" <| \_ -> expectError "cannot evaluate a spread" <| eval ( Dict.empty, Record [ ( "x", Spread Nothing ) ] )
            , test "variant_returns_variant" <| \_ -> expectEqual ( eval ( Dict.empty, Variant ( "abc", Binop ADD ( Int 1, Int 2 ) ) ), Ok (Variant ( "abc", Int 3 )) )
            , test "float_and_float_addition_returns_float" <| \_ -> expectEqual ( eval ( Dict.empty, Binop ADD ( Float 1.0, Float 2.0 ) ), Ok (Float 3.0) )
            , test "int_and_float_addition_returns_float" <| \_ -> expectEqual ( eval ( Dict.empty, Binop ADD ( Int 1, Float 2.0 ) ), Ok (Float 3.0) )
            , test "int_and_float_division_returns_float" <| \_ -> expectEqual ( eval ( Dict.empty, Binop DIV ( Int 1, Float 2.0 ) ), Ok (Float 0.5) )
            , test "float_and_int_division_returns_float" <| \_ -> expectEqual ( eval ( Dict.empty, Binop DIV ( Float 1.0, Int 2 ) ), Ok (Float 0.5) )
            , test "int_and_int_division_returns_float" <| \_ -> expectEqual ( eval ( Dict.empty, Binop DIV ( Int 1, Int 2 ) ), Ok (Float 0.5) )
            ]
        , describe "EndToEndTests"
            [ test "int_returns_int" <| \_ -> expectEqual ( run "1", Ok (Int 1) )
            , test "float_returns_float" <| \_ -> expectEqual ( run "3.14", Ok (Float 3.14) )
            , test "bytes_returns_bytes" <| \_ -> expectEqual ( run "~~QUJD", Ok (Bytes "ABC") )
            , test "bytes_base85_returns_bytes" <| \_ -> expectEqual ( run "~~85'K|(_", Ok (Bytes "ABC") )
            , test "bytes_base64_returns_bytes" <| \_ -> expectEqual ( run "~~64'QUJD", Ok (Bytes "ABC") )
            , test "bytes_base32_returns_bytes" <| \_ -> expectEqual ( run "~~32'IFBEG===", Ok (Bytes "ABC") )
            , test "bytes_base16_returns_bytes" <| \_ -> expectEqual ( run "~~16'414243", Ok (Bytes "ABC") )
            , test "int_add_returns_int" <| \_ -> expectEqual ( run "1 + 2", Ok (Int 3) )
            , test "int_sub_returns_int" <| \_ -> expectEqual ( run "1 - 2", Ok (Int -1) )
            , test "string_concat_returns_string" <| \_ -> expectEqual ( run """"abc" ++ "def" """, Ok (String "abcdef") )
            , test "list_cons_returns_list" <| \_ -> expectEqual ( run "1 >+ [2,3]", Ok (List [ Int 1, Int 2, Int 3 ]) )
            , test "list_cons_nested_returns_list" <| \_ -> expectEqual ( run "1 >+ 2 >+ [3,4]", Ok (List [ Int 1, Int 2, Int 3, Int 4 ]) )
            , test "list_append_returns_list" <| \_ -> expectEqual ( run "[1,2] +< 3", Ok (List [ Int 1, Int 2, Int 3 ]) )
            , test "list_append_nested_returns_list" <| \_ -> expectEqual ( run "[1,2] +< 3 +< 4", Ok (List [ Int 1, Int 2, Int 3, Int 4 ]) )
            , test "empty_list" <| \_ -> expectEqual ( run "[ ]", Ok (List []) )
            , test "empty_list_with_no_spaces" <| \_ -> expectEqual ( run "[]", Ok (List []) )
            , test "list_of_ints" <| \_ -> expectEqual ( run "[ 1 , 2 ]", Ok (List [ Int 1, Int 2 ]) )
            , test "list_of_exprs" <| \_ -> expectEqual ( run "[ 1 + 2 , 3 + 4 ]", Ok (List [ Int 3, Int 7 ]) )
            , test "where" <| \_ -> expectEqual ( run "a + 2 . a = 1", Ok (Int 3) )
            , test "nested_where" <| \_ -> expectEqual ( run "a + b . a = 1 . b = 2", Ok (Int 3) )
            , test "assert_with_truthy_cond_returns_value" <| \_ -> expectEqual ( run "a + 1 ? a == 1 . a = 1", Ok (Int 2) )
            , test "assert_with_falsey_cond_raises_assertion_error" <| \_ -> expectError "condition a == 2 failed" <| run "a + 1 ? a == 2 . a = 1"
            , test "nested_assert" <| \_ -> expectEqual ( run "a + b ? a == 1 ? b == 2 . a = 1 . b = 2", Ok (Int 3) )
            , test "hole" <| \_ -> expectEqual ( run "()", Ok Hole )
            , test "bindings_behave_like_letstar" <| \_ -> expectError "name 'a' is not defined" <| run "b . a = 1 . b = a"
            , test "function_application_two_args" <| \_ -> expectEqual ( run "(a -> b -> a + b) 3 2", Ok (Int 5) )
            , test "function_create_list_correct_order" <| \_ -> expectEqual ( run "(a -> b -> [a, b]) 3 2", Ok (List [ Int 3, Int 2 ]) )
            , test "create_record" <| \_ -> expectEqual ( run "{a = 1 + 3}", Ok (Record [ ( "a", Int 4 ) ]) )
            , test "access_record" <| \_ -> expectEqual ( run """rec@b . rec = { a = 1, b = "x" }""", Ok (String "x") )
            , test "access_list" <| \_ -> expectEqual ( run "xs@1 . xs = [1, 2, 3]", Ok (Int 2) )
            , test "access_list_var" <| \_ -> expectEqual ( run "xs@y . y = 2 . xs = [1, 2, 3]", Ok (Int 3) )
            , test "access_list_expr" <| \_ -> expectEqual ( run "xs@(1+1) . xs = [1, 2, 3]", Ok (Int 3) )
            , test "access_list_closure_var" <| \_ -> expectEqual ( run "list_at 1 [1,2,3] . list_at = idx -> ls -> ls@idx", Ok (Int 2) )
            , test "functions_eval_arguments" <| \_ -> expectEqual ( run "(x -> x) c . c = 1", Ok (Int 1) )
            , test "non_var_function_arg_raises_parse_error" <| \_ -> expectError "expected variable in function definition 1" <| run "1 -> a"
            , test "compose" <| \_ -> expectEqual ( run "((a -> a + 1) >> (b -> b * 2)) 3", Ok (Int 8) )
            , test "compose_does_not_expose_internal_x" <| \_ -> expectError "name 'x' is not defined" <| run "f 3 . f = ((y -> x) >> (z -> x))"
            , test "double_compose" <| \_ -> expectEqual ( run "((a -> a + 1) >> (x -> x) >> (b -> b * 2)) 3", Ok (Int 8) )
            , test "reverse_compose" <| \_ -> expectEqual ( run "((a -> a + 1) << (b -> b * 2)) 3", Ok (Int 7) )
            , test "simple_int_match" <| \_ -> expectEqual ( run """inc 2 . inc = | 1 -> 2 | 2 -> 3""", Ok (Int 3) )
            , test "var_binds_var" <| \_ -> expectEqual ( run """id 3 . id = | x -> x""", Ok (Int 3) )
            , test "var_binds_first_arm" <| \_ -> expectEqual ( run """id 3 . id = | x -> x | y -> y * 2""", Ok (Int 3) )
            , test "function_can_close_over_variables" <| \_ -> expectEqual ( run """f 1 2 . f = a -> | b -> a + b""", Ok (Int 3) )
            , test "record_binds_var" <| \_ -> expectEqual ( run """get_x rec . rec = { x = 3 } . get_x = | { x = x } -> x""", Ok (Int 3) )
            , test "record_binds_vars" <| \_ -> expectEqual ( run """mult rec . rec = { x = 3, y = 4 } . mult = | { x = x, y = y } -> x * y""", Ok (Int 12) )
            , test "record_with_extra_fields_does_not_match" <| \_ -> expectError "" <| run """mult rec . rec = { x = 3 } . mult = | { x = x, y = y } -> x * y"""
            , test "record_with_constant" <| \_ -> expectEqual ( run """mult rec . rec = { x = 4, y = 5 } . mult = | { x = 3, y = y } -> 1 | { x = 4, y = y } -> 2""", Ok (Int 2) )
            , test "record_with_non_record_fails" <| \_ -> expectError "" <| run """get_x 3 . get_x = | { x = x } -> x"""
            , test "record_doubly_binds_vars" <| \_ -> expectEqual ( run """get_x rec . rec = { a = 3, b = 3 } . get_x = | { a = x, b = x } -> x""", Ok (Int 3) )
            , test "record_spread_binds_spread" <| \_ -> expectEqual ( run "(| { a=1, ...rest } -> rest) {a=1, b=2, c=3}", Ok (Record [ ( "b", Int 2 ), ( "c", Int 3 ) ]) )
            , test "list_binds_vars" <| \_ -> expectEqual ( run """mult xs . xs = [1, 2, 3, 4, 5] . mult = | [1, x, 3, y, 5] -> x * y""", Ok (Int 8) )
            , test "list_incorrect_length_does_not_match" <| \_ -> expectError "" <| run """mult xs . xs = [1, 2, 3] . mult = | [1, 2] -> 1 | [1, 2, 3, 4] -> 1 | [1, 3] -> 1"""
            , test "list_with_constant" <| \_ -> expectEqual ( run """middle xs . xs = [4, 5, 6] . middle = | [1, x, 3] -> x | [4, x, 6] -> x | [7, x, 9] -> x""", Ok (Int 5) )
            , test "list_with_non_list_fails" <| \_ -> expectError "" <| run """get_x 3 . get_x = | [2, x] -> x"""
            , test "list_doubly_binds_vars" <| \_ -> expectEqual ( run """mult xs . xs = [1, 2, 3, 2, 1] . mult = | [1, x, 3, x, 1] -> x""", Ok (Int 2) )
            , test "list_spread_binds_spread" <| \_ -> expectEqual ( run "(| [x, ...xs] -> xs) (Ok [1, 2]", Ok (List [ Int 2 ]) )
            , test "pipe" <| \_ -> expectEqual ( run "1 |> (a -> a + 2)", Ok (Int 3) )
            , test "pipe_nested" <| \_ -> expectEqual ( run "1 |> (a -> a + 2) |> (b -> b * 2)", Ok (Int 6) )
            , test "reverse_pipe" <| \_ -> expectEqual ( run "(a -> a + 2) <| 1", Ok (Int 3) )
            , test "reverse_pipe_nested" <| \_ -> expectEqual ( run "(b -> b * 2) <| (a -> a + 2) <| 1", Ok (Int 6) )
            , test "function_can_reference_itself" <| \_ -> expectEqual ( run """f 1 . f = n -> f""", Ok (Closure ( Dict.fromList [ ( "f", Function ( Var "n", Var "f" ) ) ], Function ( Var "n", Var "f" ) )) )
            , test "function_can_call_itself" <| \_ -> expectError "" <| run """f 1 . f = n -> f n"""
            , test "function_can_call_itself" <| \_ -> expectEqual ( run """fac 5 . fac = | 0 -> 1 | 1 -> 1 | n -> n * fac (n - 1)""", Ok (Int 120) )
            , test "list_access_binds_tighter_than_append" <| \_ -> expectEqual ( run "[1, 2, 3] +< xs@0 . xs = [4]", Ok (List [ Int 1, Int 2, Int 3, Int 4 ]) )
            , test "exponentiation" <| \_ -> expectEqual ( run "6 ^ 2", Ok (Int 36) )
            , test "modulus" <| \_ -> expectEqual ( run "11 % 3", Ok (Int 2) )
            , test "exp_binds_tighter_than_mul" <| \_ -> expectEqual ( run "5 * 2 ^ 3", Ok (Int 40) )
            , test "variant_true_returns_true" <| \_ -> expectEqual ( run "# true ()", Ok true )
            , test "variant_false_returns_false" <| \_ -> expectEqual ( run "#false ()", Ok false )
            , test "boolean_and_binds_tighter_than_or" <| \_ -> expectEqual ( run "#true () || #true () && boom", Ok true )
            , test "compare_binds_tighter_than_boolean_and" <| \_ -> expectEqual ( run "1 < 2 && 2 < 1", Ok false )
            , test "list_spread" <| \_ -> expectEqual ( run """f [2, 4, 6] . f = | [] -> 0 | [x, ...] -> x | c -> 1""", Ok (Int 2) )
            , test "list_named_spread" <| \_ -> expectEqual ( run """tail [1,2,3] . tail = | [first, ...rest] -> rest""", Ok (List [ Int 2, Int 3 ]) )
            , test "record_spread" <| \_ -> expectEqual ( run """f {x = 4, y = 5} . f = | {} -> 0 | {x = a, ...} -> a | c -> 1""", Ok (Int 4) )
            , test "expr_as_boolean_variants" <| \_ -> expectEqual ( run """say (1 < 2) . say = | #false () -> "oh no" | #true () -> "omg" """, Ok (String "omg") )
            , test "variant_record" <| \_ -> expectEqual ( run """f #add {x = 3, y = 4} . f = | # b () -> "foo" | #add {x = x, y = y} -> x + y""", Ok (Int 7) )
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
