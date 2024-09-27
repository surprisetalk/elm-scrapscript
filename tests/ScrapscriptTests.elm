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


eval : String -> Result String Scrap
eval =
    run Dict.empty


suite : Test
suite =
    describe "Scrapscript"
        [ describe "Lexing"
            [ test "digit" <| \_ -> parse "1" |> Expect.equal (Ok (Int 1))
            , test "multiple_digits" <| \_ -> parse "123" |> Expect.equal (Ok (Int 123))
            , test "negative_int" <| \_ -> parse "-123" |> Expect.equal (Ok (Int -123))
            , test "float" <| \_ -> parse "3.14" |> Expect.equal (Ok (Float 3.14))
            , test "negative_float" <| \_ -> parse "-3.14" |> Expect.equal (Ok (Float -3.14))
            , test "float_with_no_integer_part" <| \_ -> parse ".14" |> Expect.equal (Ok (Float 0.14))
            , test "float_with_no_decimal_part" <| \_ -> parse "10." |> Expect.equal (Ok (Float 10.0))
            , test "binop" <| \_ -> parse "1 + 2" |> Expect.equal (Ok (Binop "+" (Int 1) (Int 2)))
            , test "binop_no_spaces" <| \_ -> parse "1+2" |> Expect.equal (Ok (Binop "+" (Int 1) (Int 2)))
            , test "two_oper_chars_returns_two_ops" <| \_ -> parse ",:" |> Expect.equal (Ok (Binop "," (Var "") (Var ":")))
            , test "binary_sub_no_spaces" <| \_ -> parse "1-2" |> Expect.equal (Ok (Binop "-" (Int 1) (Int 2)))
            , test "var" <| \_ -> parse "abc" |> Expect.equal (Ok (Var "abc"))
            , test "var_with_quote" <| \_ -> parse "sha1'abc" |> Expect.equal (Ok (Var "sha1'abc"))
            , test "dollar_sha1_var" <| \_ -> parse "$sha1'foo" |> Expect.equal (Ok (Var "$sha1'foo"))
            , test "dollar_dollar_var" <| \_ -> parse "$$bills" |> Expect.equal (Ok (Var "$$bills"))
            , test "spread" <| \_ -> parse "..." |> Expect.equal (Ok (Spread Nothing))
            , test "ignore_whitespace" <| \_ -> parse "1\n+\t2" |> Expect.equal (Ok (Binop "+" (Int 1) (Int 2)))
            , test "string" <| \_ -> parse "\"hello\"" |> Expect.equal (Ok (String "hello"))
            , test "string_with_spaces" <| \_ -> parse "\"hello world\"" |> Expect.equal (Ok (String "hello world"))
            , test "empty_list" <| \_ -> parse "[ ]" |> Expect.equal (Ok (List []))
            , test "empty_list_with_spaces" <| \_ -> parse "[ ]" |> Expect.equal (Ok (List []))
            , test "list_with_items" <| \_ -> parse "[ 1 , 2 ]" |> Expect.equal (Ok (List [ Int 1, Int 2 ]))
            , test "list_with_no_spaces" <| \_ -> parse "[1,2]" |> Expect.equal (Ok (List [ Int 1, Int 2 ]))
            , test "function" <| \_ -> parse "a -> b -> a + b" |> Expect.equal (Ok (Function (Var "a") (Function (Var "b") (Binop "+" (Var "a") (Var "b")))))
            , test "function_with_no_spaces" <| \_ -> parse "a->b->a+b" |> Expect.equal (Ok (Function (Var "a") (Function (Var "b") (Binop "+" (Var "a") (Var "b")))))
            , test "where" <| \_ -> parse "a . b" |> Expect.equal (Ok (Where (Var "a") (Var "b")))
            , test "assert" <| \_ -> parse "a ? b" |> Expect.equal (Ok (Assert (Var "a") (Var "b")))
            , test "hastype" <| \_ -> parse "a : b" |> Expect.equal (Ok (Binop ":" (Var "a") (Var "b")))
            , test "minus_returns_minus" <| \_ -> parse "-" |> Expect.equal (Ok (Var "-"))
            , test "tilde_tilde_returns_empty_bytes" <| \_ -> parse "~~" |> Expect.equal (Ok (Bytes ""))
            , test "bytes_returns_bytes_base64" <| \_ -> parse "~~QUJD" |> Expect.equal (Ok (Bytes "QUJD"))
            , test "bytes_base85" <| \_ -> parse "~~85'K|(_" |> Expect.equal (Ok (Bytes "85'K|(_"))
            , test "bytes_base64" <| \_ -> parse "~~64'QUJD" |> Expect.equal (Ok (Bytes "64'QUJD"))
            , test "bytes_base32" <| \_ -> parse "~~32'IFBEG===" |> Expect.equal (Ok (Bytes "32'IFBEG==="))
            , test "bytes_base16" <| \_ -> parse "~~16'414243" |> Expect.equal (Ok (Bytes "16'414243"))
            , test "hole" <| \_ -> parse "()" |> Expect.equal (Ok Hole)
            , test "hole_with_spaces" <| \_ -> parse "( )" |> Expect.equal (Ok Hole)
            , test "parenthetical_expression" <| \_ -> parse "(1+2)" |> Expect.equal (Ok (Binop "+" (Int 1) (Int 2)))
            , test "pipe" <| \_ -> parse "1 |> f . f = a -> a + 1" |> Expect.equal (Ok (Binop "|>" (Int 1) (Where (Var "f") (Binop "=" (Var "f") (Function (Var "a") (Binop "+" (Var "a") (Int 1)))))))
            , test "reverse_pipe" <| \_ -> parse "f <| 1 . f = a -> a + 1" |> Expect.equal (Ok (Binop "<|" (Var "f") (Where (Int 1) (Binop "=" (Var "f") (Function (Var "a") (Binop "+" (Var "a") (Int 1)))))))
            , test "record_no_fields" <| \_ -> parse "{ }" |> Expect.equal (Ok (Record Dict.empty))
            , test "record_no_fields_no_spaces" <| \_ -> parse "{}" |> Expect.equal (Ok (Record Dict.empty))
            , test "record_one_field" <| \_ -> parse "{ a = 4 }" |> Expect.equal (Ok (Record (Dict.fromList [ ( "a", Int 4 ) ])))
            , test "record_multiple_fields" <| \_ -> parse "{ a = 4, b = \"z\" }" |> Expect.equal (Ok (Record (Dict.fromList [ ( "a", Int 4 ), ( "b", String "z" ) ])))
            , test "record_access" <| \_ -> parse "r@a" |> Expect.equal (Ok (Binop "@" (Var "r") (Var "a")))
            , test "right_eval" <| \_ -> parse "a!b" |> Expect.equal (Ok (Binop "!" (Var "a") (Var "b")))
            , test "match" <| \_ -> parse "g = | 1 -> 2 | 2 -> 3" |> Expect.equal (Ok (Binop "=" (Var "g") (Binop "|" (Binop "->" (Int 1) (Int 2)) (Binop "->" (Int 2) (Int 3)))))
            , test "compose" <| \_ -> parse "f >> g" |> Expect.equal (Ok (Binop ">>" (Var "f") (Var "g")))
            , test "compose_reverse" <| \_ -> parse "f << g" |> Expect.equal (Ok (Binop "<<" (Var "f") (Var "g")))
            , test "list_with_only_spread" <| \_ -> parse "[ ... ]" |> Expect.equal (Ok (List [ Spread Nothing ]))
            , test "list_with_spread" <| \_ -> parse "[ 1 , ... ]" |> Expect.equal (Ok (List [ Int 1, Spread Nothing ]))
            , test "list_with_spread_no_spaces" <| \_ -> parse "[ 1,... ]" |> Expect.equal (Ok (List [ Int 1, Spread Nothing ]))
            , test "list_with_named_spread" <| \_ -> parse "[1,...rest]" |> Expect.equal (Ok (List [ Int 1, Spread (Just "rest") ]))
            , test "record_with_only_spread" <| \_ -> parse "{ ... }" |> Expect.equal (Ok (Record (Dict.fromList [ ( "...", Spread Nothing ) ])))
            , test "record_with_spread" <| \_ -> parse "{ x = 1, ...}" |> Expect.equal (Ok (Record (Dict.fromList [ ( "x", Int 1 ), ( "...", Spread Nothing ) ])))
            , test "record_with_spread_no_spaces" <| \_ -> parse "{x=1,...}" |> Expect.equal (Ok (Record (Dict.fromList [ ( "x", Int 1 ), ( "...", Spread Nothing ) ])))
            , test "variant_with_space" <| \_ -> parse "# abc" |> Expect.equal (Ok (Variant "abc" Hole))
            , test "variant_with_no_space" <| \_ -> parse "#abc" |> Expect.equal (Ok (Variant "abc" Hole))
            ]
        , describe "Parsing"
            [ test "with_empty_tokens_raises_parse_error" <| \_ -> parse "" |> Expect.equal (Err "unexpected end of input")
            , test "digit_returns_int" <| \_ -> parse "1" |> Expect.equal (Ok (Int 1))
            , test "digits_returns_int" <| \_ -> parse "123" |> Expect.equal (Ok (Int 123))
            , test "negative_int_returns_binary_sub_int" <| \_ -> parse "-123" |> Expect.equal (Ok (Int -123))
            , test "negative_var_returns_binary_sub_int" <| \_ -> parse "-x" |> Expect.equal (Ok (Binop "-" (Int 0) (Var "x")))
            , test "negative_int_binds_tighter_than_plus" <| \_ -> parse "-l + r" |> Expect.equal (Ok (Binop "+" (Binop "-" (Int 0) (Var "l")) (Var "r")))
            , test "negative_int_binds_tighter_than_mul" <| \_ -> parse "-l * r" |> Expect.equal (Ok (Binop "*" (Binop "-" (Int 0) (Var "l")) (Var "r")))
            , test "negative_int_binds_tighter_than_index" <| \_ -> parse "-l @ r" |> Expect.equal (Ok (Binop "@" (Binop "-" (Int 0) (Var "l")) (Var "r")))
            , test "negative_int_binds_tighter_than_apply" <| \_ -> parse "-l r" |> Expect.equal (Ok (Apply (Binop "-" (Int 0) (Var "l")) (Var "r")))
            , test "decimal_returns_float" <| \_ -> parse "3.14" |> Expect.equal (Ok (Float 3.14))
            , test "negative_float_returns_binary_sub_float" <| \_ -> parse "-3.14" |> Expect.equal (Ok (Binop "-" (Int 0) (Float 3.14)))
            , test "var_returns_var" <| \_ -> parse "abc_123" |> Expect.equal (Ok (Var "abc_123"))
            , test "sha_var_returns_var" <| \_ -> parse "$sha1'abc" |> Expect.equal (Ok (Var "$sha1'abc"))
            , test "sha_var_without_quote_returns_var" <| \_ -> parse "$sha1abc" |> Expect.equal (Ok (Var "$sha1abc"))
            , test "dollar_returns_var" <| \_ -> parse "$" |> Expect.equal (Ok (Var "$"))
            , test "dollar_dollar_returns_var" <| \_ -> parse "$$" |> Expect.equal (Ok (Var "$$"))
            , test "sha_var_without_dollar_raises_parse_error" <| \_ -> parse "sha1'abc" |> Expect.equal (Err "unexpected token")
            , test "dollar_dollar_var_returns_var" <| \_ -> parse "$$bills" |> Expect.equal (Ok (Var "$$bills"))
            , test "bytes_returns_bytes" <| \_ -> parse "~~QUJD" |> Expect.equal (Ok (Bytes "ABC"))
            , test "binary_add_returns_binop" <| \_ -> parse "1 + 2" |> Expect.equal (Ok (Binop "+" (Int 1) (Int 2)))
            , test "binary_sub_returns_binop" <| \_ -> parse "1 - 2" |> Expect.equal (Ok (Binop "-" (Int 1) (Int 2)))
            , test "binary_add_right_returns_binop" <| \_ -> parse "1 + 2 + 3" |> Expect.equal (Ok (Binop "+" (Int 1) (Binop "+" (Int 2) (Int 3))))
            , test "mul_binds_tighter_than_add_right" <| \_ -> parse "1 + 2 * 3" |> Expect.equal (Ok (Binop "+" (Int 1) (Binop "*" (Int 2) (Int 3))))
            , test "mul_binds_tighter_than_add_left" <| \_ -> parse "1 * 2 + 3" |> Expect.equal (Ok (Binop "+" (Binop "*" (Int 1) (Int 2)) (Int 3)))
            , test "mul_and_div_bind_left_to_right" <| \_ -> parse "1 / 3 * 3" |> Expect.equal (Ok (Binop "*" (Binop "/" (Int 1) (Int 3)) (Int 3)))
            , test "exp_binds_tighter_than_mul_right" <| \_ -> parse "5 * 2 ^ 3" |> Expect.equal (Ok (Binop "*" (Int 5) (Binop "^" (Int 2) (Int 3))))
            , test "list_access_binds_tighter_than_append" <| \_ -> parse "a +< ls @ 0" |> Expect.equal (Ok (Binop "+<" (Var "a") (Binop "@" (Var "ls") (Int 0))))
            , test "binary_str_concat_returns_binop" <| \_ -> parse "\"abc\" ++ \"def\"" |> Expect.equal (Ok (Binop "++" (String "abc") (String "def")))
            , test "binary_list_cons_returns_binop" <| \_ -> parse "a >+ b" |> Expect.equal (Ok (Binop ">+" (Var "a") (Var "b")))
            , test "binary_list_append_returns_binop" <| \_ -> parse "a +< b" |> Expect.equal (Ok (Binop "+<" (Var "a") (Var "b")))
            , test "empty_list" <| \_ -> parse "[]" |> Expect.equal (Ok (List []))
            , test "list_of_ints_returns_list" <| \_ -> parse "[1, 2]" |> Expect.equal (Ok (List [ Int 1, Int 2 ]))
            , test "list_with_only_comma_raises_parse_error" <| \_ -> parse "[,]" |> Expect.equal (Err "unexpected token Operator (lineno=-1, value=',')")
            , test "list_with_two_commas_raises_parse_error" <| \_ -> parse "[,,]" |> Expect.equal (Err "unexpected token Operator (lineno=-1, value=',')")
            , test "list_with_trailing_comma_raises_parse_error" <| \_ -> parse "[1,]" |> Expect.equal (Err "unexpected token RightBracket (lineno=-1)")
            , test "assign" <| \_ -> parse "a = 1" |> Expect.equal (Ok (Binop "=" (Var "a") (Int 1)))
            , test "function_one_arg_returns_function" <| \_ -> parse "a -> a + 1" |> Expect.equal (Ok (Function (Var "a") (Binop "+" (Var "a") (Int 1))))
            , test "function_two_args_returns_functions" <| \_ -> parse "a -> b -> a + b" |> Expect.equal (Ok (Function (Var "a") (Function (Var "b") (Binop "+" (Var "a") (Var "b")))))
            , test "assign_function" <| \_ -> parse "id = x -> x" |> Expect.equal (Ok (Binop "=" (Var "id") (Function (Var "x") (Var "x"))))
            , test "function_application_one_arg" <| \_ -> parse "f a" |> Expect.equal (Ok (Apply (Var "f") (Var "a")))
            , test "function_application_two_args" <| \_ -> parse "f a b" |> Expect.equal (Ok (Apply (Apply (Var "f") (Var "a")) (Var "b")))
            , test "where" <| \_ -> parse "a . b" |> Expect.equal (Ok (Where (Var "a") (Var "b")))
            , test "nested_where" <| \_ -> parse "a . b . c" |> Expect.equal (Ok (Where (Where (Var "a") (Var "b")) (Var "c")))
            , test "assert" <| \_ -> parse "a ? b" |> Expect.equal (Ok (Assert (Var "a") (Var "b")))
            , test "nested_assert" <| \_ -> parse "a ? b ? c" |> Expect.equal (Ok (Assert (Assert (Var "a") (Var "b")) (Var "c")))
            , test "mixed_assert_where" <| \_ -> parse "a ? b . c" |> Expect.equal (Ok (Where (Assert (Var "a") (Var "b")) (Var "c")))
            , test "hastype" <| \_ -> parse "a : b" |> Expect.equal (Ok (Binop ":" (Var "a") (Var "b")))
            , test "hole" <| \_ -> parse "()" |> Expect.equal (Ok Hole)
            , test "parenthesized_expression" <| \_ -> parse "(1 + 2)" |> Expect.equal (Ok (Binop "+" (Int 1) (Int 2)))
            , test "parenthesized_add_mul" <| \_ -> parse "(1 + 2) * 3" |> Expect.equal (Ok (Binop "*" (Binop "+" (Int 1) (Int 2)) (Int 3)))
            , test "pipe" <| \_ -> parse "1 |> f" |> Expect.equal (Ok (Apply (Var "f") (Int 1)))
            , test "nested_pipe" <| \_ -> parse "1 |> f |> g" |> Expect.equal (Ok (Apply (Var "g") (Apply (Var "f") (Int 1))))
            , test "reverse_pipe" <| \_ -> parse "f <| 1" |> Expect.equal (Ok (Apply (Var "f") (Int 1)))
            , test "nested_reverse_pipe" <| \_ -> parse "g <| f <| 1" |> Expect.equal (Ok (Apply (Var "g") (Apply (Var "f") (Int 1))))
            , test "empty_record" <| \_ -> parse "{}" |> Expect.equal (Ok (record []))
            , test "record_single_field" <| \_ -> parse "{a = 4}" |> Expect.equal (Ok (record [ ( "a", Int 4 ) ]))
            , test "record_with_expression" <| \_ -> parse "{a = 1 + 2}" |> Expect.equal (Ok (record [ ( "a", Binop "+" (Int 1) (Int 2) ) ]))
            , test "record_multiple_fields" <| \_ -> parse "{a = 4, b = \"z\"}" |> Expect.equal (Ok (record [ ( "a", Int 4 ), ( "b", String "z" ) ]))
            , test "non_variable_in_assignment_raises_parse_error" <| \_ -> parse "3 = 4" |> Expect.equal (Err "expected variable in assignment Int (value=3)")
            , test "non_assign_in_record_constructor_raises_parse_error" <| \_ -> Expect.equal (parse "{1, 2}") (Err "failed to parse variable assignment in record constructor")
            , test "right_eval_returns_binop" <| \_ -> parse "a ! b" |> Expect.equal (Ok (Binop "!" (Var "a") (Var "b")))
            , test "right_eval_with_defs_returns_binop" <| \_ -> parse "a ! b . c" |> Expect.equal (Ok (Binop "!" (Var "a") (Where (Var "b") (Var "c"))))
            , test "no_cases_raises_parse_error" <| \_ -> Expect.equal (parse "|") (Err "unexpected end of input")
            , test "one_case" <| \_ -> parse "| 1 -> 2" |> Expect.equal (Err "TODO")
            , test "two_cases" <| \_ -> parse "| 1 -> 2 | 2 -> 3" |> Expect.equal (Err "TODO")
            , test "compose" <| \_ -> parse "f >> g" |> Expect.equal (Ok (Function (Var "$v0") (Apply (Var "g") (Apply (Var "f") (Var "$v0")))))
            , test "compose_reverse" <| \_ -> parse "f << g" |> Expect.equal (Ok (Function (Var "$v0") (Apply (Var "f") (Apply (Var "g") (Var "$v0")))))
            , test "double_compose" <| \_ -> parse "f << g << h" |> Expect.equal (Ok (Function (Var "$v1") (Apply (Var "f") (Apply (Function (Var "$v0") (Apply (Var "g") (Apply (Var "h") (Var "$v0")))) (Var "$v1")))))
            , test "boolean_and_binds_tighter_than_or" <| \_ -> parse "x || y && z" |> Expect.equal (Ok (Binop "||" (Var "x") (Binop "&&" (Var "y") (Var "z"))))
            , test "list_spread" <| \_ -> parse "[1, ...]" |> Expect.equal (Ok (List [ Int 1, Spread Nothing ]))
            , test "list_with_non_name_expr_after_spread_raises_parse_error" <| \_ -> parse "[1, ..., 2]" |> Expect.equal (Err "unexpected token IntLit (lineno=-1, value=1)")
            , test "list_with_named_spread" <| \_ -> parse "[1, ...rest]" |> Expect.equal (Ok (List [ Int 1, Spread (Just "rest") ]))
            , test "list_spread_beginning_raises_parse_error" <| \_ -> parse "[..., 1]" |> Expect.equal (Err "spread must come at end of list match")
            , test "list_named_spread_beginning_raises_parse_error" <| \_ -> parse "[...rest, 1]" |> Expect.equal (Err "spread must come at end of list match")
            , test "list_spread_middle_raises_parse_error" <| \_ -> parse "[1, ..., 1]" |> Expect.equal (Err "spread must come at end of list match")
            , test "list_named_spread_middle_raises_parse_error" <| \_ -> parse "[1, ...rest, 1]" |> Expect.equal (Err "spread must come at end of list match")
            , test "record_spread" <| \_ -> parse "{x = 1, ...}" |> Expect.equal (Ok (record [ ( "x", Int 1 ), ( "...", Spread Nothing ) ]))
            , test "record_spread_beginning_raises_parse_error" <| \_ -> parse "{..., x = 1}" |> Expect.equal (Err "spread must come at end of record match")
            , test "record_spread_middle_raises_parse_error" <| \_ -> parse "{x = 1, ..., y = 2}" |> Expect.equal (Err "spread must come at end of record match")
            , test "record_with_only_comma_raises_parse_error" <| \_ -> parse "{,}" |> Expect.equal (Err "unexpected token Operator (lineno=-1, value=',')")
            , test "record_with_two_commas_raises_parse_error" <| \_ -> parse "{,,}" |> Expect.equal (Err "unexpected token Operator (lineno=-1, value=',')")
            , test "record_with_trailing_comma_raises_parse_error" <| \_ -> parse "{x = 1,}" |> Expect.equal (Err "unexpected token RightBrace (lineno=-1)")
            , test "variant_returns_variant" <| \_ -> parse "#abc 1" |> Expect.equal (Ok (Variant "abc" (Int 1)))
            , test "with_variant" <| \_ -> parse "| #true () -> 123" |> Expect.equal (Err "TODO")
            , test "binary_and_with_variant_args" <| \_ -> parse "#true () && #false ()" |> Expect.equal (Ok (Binop "&&" true false))
            , test "apply_with_variant_args" <| \_ -> parse "f #true () #false ()" |> Expect.equal (Ok (Apply (Apply (Var "f") true) false))
            ]
        , describe "Eval"
            [ test "int_returns_int" <| \_ -> eval "5" |> Expect.equal (Ok (Int 5))
            , test "float_returns_float" <| \_ -> eval "3.14" |> Expect.equal (Ok (Float 3.14))
            , test "str_returns_str" <| \_ -> eval "\"xyz\"" |> Expect.equal (Ok (String "xyz"))
            , test "bytes_returns_bytes" <| \_ -> eval "~~xyz" |> Expect.equal (Ok (Bytes "xyz"))
            , test "with_non_existent_var_raises_name_error" <| \_ -> eval "no" |> Expect.equal (Err "TODO")
            , test "with_bound_var_returns_value" <| \_ -> run (Dict.fromList [ ( "yes", Int 123 ) ]) "yes" |> Expect.equal (Ok (Int 123))
            , test "with_binop_add_returns_sum" <| \_ -> eval "1 + 2" |> Expect.equal (Ok (Int 3))
            , test "with_nested_binop" <| \_ -> eval "(1 + 2) + 3" |> Expect.equal (Ok (Int 6))
            , test "with_binop_add_with_int_string_raises_type_error" <| \_ -> eval "1 + \"hello\"" |> Expect.equal (Err "expected Int or Float, got String")
            , test "with_binop_sub" <| \_ -> eval "1 - 2" |> Expect.equal (Ok (Int -1))
            , test "with_binop_mul" <| \_ -> eval "2 * 3" |> Expect.equal (Ok (Int 6))
            , test "with_binop_div" <| \_ -> eval "3 / 10" |> Expect.equal (Ok (Float 0.3))
            , test "with_binop_floor_div" <| \_ -> eval "2 // 3" |> Expect.equal (Ok (Int 0))
            , test "with_binop_exp" <| \_ -> eval "2 ^ 3" |> Expect.equal (Ok (Int 8))
            , test "with_binop_mod" <| \_ -> eval "10 % 4" |> Expect.equal (Ok (Int 2))
            , test "with_binop_equal_with_equal_returns_true" <| \_ -> eval "1 == 1" |> Expect.equal (Ok true)
            , test "with_binop_equal_with_inequal_returns_false" <| \_ -> eval "1 == 2" |> Expect.equal (Ok false)
            , test "with_binop_not_equal_with_equal_returns_false" <| \_ -> eval "1 != 1" |> Expect.equal (Ok false)
            , test "with_binop_not_equal_with_inequal_returns_true" <| \_ -> eval "1 != 2" |> Expect.equal (Ok true)
            , test "with_binop_concat_with_strings_returns_string" <| \_ -> eval "\"hello\" ++ \" world\"" |> Expect.equal (Ok (String "hello world"))
            , test "with_binop_concat_with_int_string_raises_type_error" <| \_ -> eval "123 ++ \" world\"" |> Expect.equal (Err "expected String, got Int")
            , test "with_binop_concat_with_string_int_raises_type_error" <| \_ -> eval "\" world\" ++ 123" |> Expect.equal (Err "expected String, got Int")
            , test "with_binop_cons_with_int_list_returns_list" <| \_ -> eval "1 >+ [2, 3]" |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3 ]))
            , test "with_binop_cons_with_list_list_returns_nested_list" <| \_ -> eval "[] >+ []" |> Expect.equal (Ok (List [ List [] ]))
            , test "with_binop_cons_with_list_int_raises_type_error" <| \_ -> eval "[] >+ 123" |> Expect.equal (Err "expected List, got Int")
            , test "with_list_append" <| \_ -> eval "[1, 2] +< 3" |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3 ]))
            , test "with_list_evaluates_elements" <| \_ -> eval "[1 + 2, 3 + 4]" |> Expect.equal (Ok (List [ Int 3, Int 7 ]))
            , test "with_function_returns_closure_with_improved_env" <| \_ -> run (Dict.fromList [ ( "a", Int 1 ), ( "b", Int 2 ) ]) "x -> x" |> Expect.equal (Err "TODO")
            , test "with_match_function_returns_closure_with_improved_env" <| \_ -> run (Dict.fromList [ ( "a", Int 1 ), ( "b", Int 2 ) ]) "| " |> Expect.equal (Err "TODO")
            , test "assign_returns_env_object" <| \_ -> eval "a = 1" |> Expect.equal (Err "TODO")
            , test "where_evaluates_in_order" <| \_ -> eval "(a + 2) . a = 1" |> Expect.equal (Ok (Int 3))
            , test "nested_where" <| \_ -> eval "((a + b) . a = 1) . b = 2" |> Expect.equal (Ok (Int 3))
            , test "assert_with_truthy_cond_returns_value" <| \_ -> eval "123 ? #true ()" |> Expect.equal (Ok (Int 123))
            , test "assert_with_falsey_cond_raises_assertion_error" <| \_ -> eval "123 ? #false ()" |> Expect.equal (Err "condition #false () failed")
            , test "nested_assert" <| \_ -> eval "(123 ? #true ()) ? #true ()" |> Expect.equal (Ok (Int 123))
            , test "hole" <| \_ -> eval "()" |> Expect.equal (Ok Hole)
            , test "function_application_one_arg" <| \_ -> eval "(x -> x + 1) 2" |> Expect.equal (Ok (Int 3))
            , test "function_application_two_args" <| \_ -> eval "((a -> b -> a + b) 3) 2" |> Expect.equal (Ok (Int 5))
            , test "function_capture_env" <| \_ -> run (Dict.fromList [ ( "y", Int 5 ) ]) "(x -> x + y) 2" |> Expect.equal (Ok (Int 7))
            , test "non_function_raises_type_error" <| \_ -> eval "3 4" |> Expect.equal (Err "attempted to apply a non-closure of type Int")
            , test "access_from_invalid_object_raises_type_error" <| \_ -> eval "4 @ \"x\"" |> Expect.equal (Err "attempted to access from type Int")
            , test "record_evaluates_value_expressions" <| \_ -> eval "{a = 1 + 2}" |> Expect.equal (Ok (record [ ( "a", Int 3 ) ]))
            , test "record_access_with_invalid_accessor_raises_type_error" <| \_ -> eval "{a = 4} @ 0" |> Expect.equal (Err "cannot access record field using Int, expected a field name")
            , test "record_access_with_unknown_accessor_raises_name_error" <| \_ -> eval "{a = 4} @ b" |> Expect.equal (Err "no assignment to b found in record")
            , test "record_access" <| \_ -> eval "{a = 4} @ a" |> Expect.equal (Ok (Int 4))
            , test "list_access_with_invalid_accessor_raises_type_error" <| \_ -> eval "[4] @ \"hello\"" |> Expect.equal (Err "cannot index into list using type String, expected integer")
            , test "list_access_with_out_of_bounds_accessor_raises_value_error" <| \_ -> eval "[1, 2, 3] @ 4" |> Expect.equal (Err "index 4 out of bounds for list")
            , test "list_access" <| \_ -> eval "[\"a\", \"b\", \"c\"] @ 2" |> Expect.equal (Ok (String "c"))
            , test "right_eval_evaluates_right_hand_side" <| \_ -> eval "1 ! 2" |> Expect.equal (Ok (Int 2))
            , test "no_cases_raises_match_error" <| \_ -> eval "(|) 1" |> Expect.equal (Err "no matching cases")
            , test "int_with_equal_int_matches" <| \_ -> eval "(| 1 -> 2) 1" |> Expect.equal (Ok (Int 2))
            , test "int_with_inequal_int_raises_match_error" <| \_ -> eval "(| 1 -> 2) 3" |> Expect.equal (Err "no matching cases")
            , test "string_with_equal_string_matches" <| \_ -> eval "(| \"a\" -> \"b\") \"a\"" |> Expect.equal (Ok (String "b"))
            , test "string_with_inequal_string_raises_match_error" <| \_ -> eval "(| \"a\" -> \"b\") \"c\"" |> Expect.equal (Err "no matching cases")
            , test "falls_through_to_next" <| \_ -> eval "(| 3 -> 4 | 1 -> 2) 1" |> Expect.equal (Ok (Int 2))
            , test "apply_quote_returns_ast" <| \_ -> eval "$$quote (1 + 2)" |> Expect.equal (Ok (Binop "+" (Int 1) (Int 2)))
            , test "apply_closure_with_match_function_has_access_to_closure_vars" <| \_ -> eval "((x = 1) . | y -> x) 2" |> Expect.equal (Ok (Int 1))
            , test "less_returns_bool" <| \_ -> eval "3 < 4" |> Expect.equal (Ok true)
            , test "less_on_non_bool_raises_type_error" <| \_ -> eval "\"xyz\" < 4" |> Expect.equal (Err "expected Int or Float, got String")
            , test "less_equal_returns_bool" <| \_ -> eval "3 <= 4" |> Expect.equal (Ok true)
            , test "less_equal_on_non_bool_raises_type_error" <| \_ -> eval "\"xyz\" <= 4" |> Expect.equal (Err "expected Int or Float, got String")
            , test "greater_returns_bool" <| \_ -> eval "3 > 4" |> Expect.equal (Ok false)
            , test "greater_on_non_bool_raises_type_error" <| \_ -> eval "\"xyz\" > 4" |> Expect.equal (Err "expected Int or Float, got String")
            , test "greater_equal_returns_bool" <| \_ -> eval "3 >= 4" |> Expect.equal (Ok false)
            , test "greater_equal_on_non_bool_raises_type_error" <| \_ -> eval "\"xyz\" >= 4" |> Expect.equal (Err "expected Int or Float, got String")
            , test "boolean_and_evaluates_args_1" <| \_ -> run (Dict.fromList [ ( "a", false ) ]) "#true () && a" |> Expect.equal (Ok false)
            , test "boolean_and_evaluates_args_2" <| \_ -> run (Dict.fromList [ ( "a", true ) ]) "a && #false ()" |> Expect.equal (Ok false)
            , test "boolean_or_evaluates_args_1" <| \_ -> run (Dict.fromList [ ( "a", true ) ]) "#false () || a" |> Expect.equal (Ok true)
            , test "boolean_or_evaluates_args_2" <| \_ -> run (Dict.fromList [ ( "a", false ) ]) "a || #true ()" |> Expect.equal (Ok true)
            , test "boolean_and_on_int_raises_type_error" <| \_ -> eval "1 && 2" |> Expect.equal (Err "expected #true or #false, got Int")
            , test "boolean_or_on_int_raises_type_error" <| \_ -> eval "1 || 2" |> Expect.equal (Err "expected #true or #false, got Int")
            , test "record_with_spread_fails" <| \_ -> eval "{x = ...}" |> Expect.equal (Err "cannot evaluate a spread")
            , test "variant_returns_variant" <| \_ -> eval "#abc (1 + 2)" |> Expect.equal (Ok (Variant "abc" (Int 3)))
            , test "float_and_float_addition_returns_float" <| \_ -> eval "1.0 + 2.0" |> Expect.equal (Ok (Float 3.0))
            , test "int_and_float_addition_returns_float" <| \_ -> eval "1 + 2.0" |> Expect.equal (Ok (Float 3.0))
            , test "int_and_float_division_returns_float" <| \_ -> eval "1 / 2.0" |> Expect.equal (Ok (Float 0.5))
            , test "float_and_int_division_returns_float" <| \_ -> eval "1.0 / 2" |> Expect.equal (Ok (Float 0.5))
            , test "int_and_int_division_returns_float" <| \_ -> eval "1 / 2" |> Expect.equal (Ok (Float 0.5))
            ]
        , describe "End-to-End"
            [ test "int returns int" <| \_ -> eval "1" |> Expect.equal (Ok (Int 1))
            , test "float returns float" <| \_ -> eval "3.14" |> Expect.equal (Ok (Float 3.14))
            , test "bytes returns bytes" <| \_ -> eval "~~QUJD" |> Expect.equal (Ok (Bytes "QUJD"))
            , test "int add returns int" <| \_ -> eval "1 + 2" |> Expect.equal (Ok (Int 3))
            , test "int sub returns int" <| \_ -> eval "1 - 2" |> Expect.equal (Ok (Int -1))
            , test "string concat returns string" <| \_ -> eval "\"abc\" ++ \"def\"" |> Expect.equal (Ok (String "abcdef"))
            , test "list cons returns list" <| \_ -> eval "1 >+ [2,3]" |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3 ]))
            , test "list cons nested returns list" <| \_ -> eval "1 >+ 2 >+ [3,4]" |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3, Int 4 ]))
            , test "list append returns list" <| \_ -> eval "[1,2] +< 3" |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3 ]))
            , test "list append nested returns list" <| \_ -> eval "[1,2] +< 3 +< 4" |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3, Int 4 ]))
            , test "empty list" <| \_ -> eval "[ ]" |> Expect.equal (Ok (List []))
            , test "empty list with no spaces" <| \_ -> eval "[]" |> Expect.equal (Ok (List []))
            , test "list of ints" <| \_ -> eval "[ 1 , 2 ]" |> Expect.equal (Ok (List [ Int 1, Int 2 ]))
            , test "list of exprs" <| \_ -> eval "[ 1 + 2 , 3 + 4 ]" |> Expect.equal (Ok (List [ Int 3, Int 7 ]))
            , test "where" <| \_ -> eval "a + 2 . a = 1" |> Expect.equal (Ok (Int 3))
            , test "nested where" <| \_ -> eval "a + b . a = 1 . b = 2" |> Expect.equal (Ok (Int 3))
            , test "assert with truthy cond returns value" <| \_ -> eval "a + 1 ? a == 1 . a = 1" |> Expect.equal (Ok (Int 2))
            , test "assert with falsey cond raises assertion error" <| \_ -> eval "a + 1 ? a == 2 . a = 1" |> Expect.err
            , test "nested assert" <| \_ -> eval "a + b ? a == 1 ? b == 2 . a = 1 . b = 2" |> Expect.equal (Ok (Int 3))
            , test "hole" <| \_ -> eval "()" |> Expect.equal (Ok Hole)
            , test "bindings behave like letstar" <| \_ -> eval "b . a = 1 . b = a" |> Expect.err
            , test "function application two args" <| \_ -> eval "(a -> b -> a + b) 3 2" |> Expect.equal (Ok (Int 5))
            , test "function create list correct order" <| \_ -> eval "(a -> b -> [a, b]) 3 2" |> Expect.equal (Ok (List [ Int 3, Int 2 ]))
            , test "create record" <| \_ -> eval "{a = 1 + 3}" |> Expect.equal (Ok (Record (Dict.fromList [ ( "a", Int 4 ) ])))
            , test "access record" <| \_ -> eval "rec@b . rec = { a = 1, b = \"x\" }" |> Expect.equal (Ok (String "x"))
            , test "access list" <| \_ -> eval "xs@1 . xs = [1, 2, 3]" |> Expect.equal (Ok (Int 2))
            , test "access list var" <| \_ -> eval "xs@y . y = 2 . xs = [1, 2, 3]" |> Expect.equal (Ok (Int 3))
            , test "access list expr" <| \_ -> eval "xs@(1+1) . xs = [1, 2, 3]" |> Expect.equal (Ok (Int 3))
            , test "access list closure var" <| \_ -> eval "list_at 1 [1,2,3] . list_at = idx -> ls -> ls@idx" |> Expect.equal (Ok (Int 2))
            , test "functions eval arguments" <| \_ -> eval "(x -> x) c . c = 1" |> Expect.equal (Ok (Int 1))
            , test "non var function arg raises parse error" <| \_ -> eval "1 -> a" |> Expect.err
            , test "compose" <| \_ -> eval "((a -> a + 1) >> (b -> b * 2)) 3" |> Expect.equal (Ok (Int 8))
            , test "compose does not expose internal x" <| \_ -> eval "f 3 . f = ((y -> x) >> (z -> x))" |> Expect.err
            , test "double compose" <| \_ -> eval "((a -> a + 1) >> (x -> x) >> (b -> b * 2)) 3" |> Expect.equal (Ok (Int 8))
            , test "reverse compose" <| \_ -> eval "((a -> a + 1) << (b -> b * 2)) 3" |> Expect.equal (Ok (Int 7))
            , test "match list spread binds spread" <| \_ -> eval "(| [x, ...xs] -> xs) [1, 2]" |> Expect.equal (Ok (List [ Int 2 ]))
            , test "pipe" <| \_ -> eval "1 |> (a -> a + 2)" |> Expect.equal (Ok (Int 3))
            , test "pipe nested" <| \_ -> eval "1 |> (a -> a + 2) |> (b -> b * 2)" |> Expect.equal (Ok (Int 6))
            , test "reverse pipe" <| \_ -> eval "(a -> a + 2) <| 1" |> Expect.equal (Ok (Int 3))
            , test "reverse pipe nested" <| \_ -> eval "(b -> b * 2) <| (a -> a + 2) <| 1" |> Expect.equal (Ok (Int 6))
            , test "list access binds tighter than append" <| \_ -> eval "[1, 2, 3] +< xs@0 . xs = [4]" |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3, Int 4 ]))
            , test "exponentiation" <| \_ -> eval "6 ^ 2" |> Expect.equal (Ok (Int 36))
            , test "modulus" <| \_ -> eval "11 % 3" |> Expect.equal (Ok (Int 2))
            , test "exp binds tighter than mul" <| \_ -> eval "5 * 2 ^ 3" |> Expect.equal (Ok (Int 40))
            , test "variant true returns true" <| \_ -> eval "# true ()" |> Expect.equal (Ok (Variant "true" Hole))
            , test "variant false returns false" <| \_ -> eval "#false ()" |> Expect.equal (Ok (Variant "false" Hole))
            , test "boolean and binds tighter than or" <| \_ -> eval "#true () || #true () && boom" |> Expect.equal (Ok (Variant "true" Hole))
            , test "compare binds tighter than boolean and" <| \_ -> eval "1 < 2 && 2 < 1" |> Expect.equal (Ok (Variant "false" Hole))
            , test "match record spread binds spread" <| \_ -> eval "(| { a=1, ...rest } -> rest) {a=1, b=2, c=3}" |> Expect.equal (Ok (Record (Dict.fromList [ ( "b", Int 2 ), ( "c", Int 3 ) ])))
            , test "simple int match" <|
                \_ ->
                    eval """
                inc 2
                . inc =
                  | 1 -> 2
                  | 2 -> 3
                """
                        |> Expect.equal (Ok (Int 3))
            , test "match var binds var" <|
                \_ ->
                    eval """
                id 3
                . id =
                  | x -> x
                """
                        |> Expect.equal (Ok (Int 3))
            , test "match var binds first arm" <|
                \_ ->
                    eval """
                id 3
                . id =
                  | x -> x
                  | y -> y * 2
                """
                        |> Expect.equal (Ok (Int 3))
            , test "match function can close over variables" <|
                \_ ->
                    eval """
                f 1 2
                . f = a ->
                  | b -> a + b
                """
                        |> Expect.equal (Ok (Int 3))
            , test "match record binds var" <|
                \_ ->
                    eval """
                get_x rec
                . rec = { x = 3 }
                . get_x =
                  | { x = x } -> x
                """
                        |> Expect.equal (Ok (Int 3))
            , test "match record binds vars" <|
                \_ ->
                    eval """
                mult rec
                . rec = { x = 3, y = 4 }
                . mult =
                  | { x = x, y = y } -> x * y
                """
                        |> Expect.equal (Ok (Int 12))
            , test "match record with extra fields does not match" <|
                \_ ->
                    eval """
                mult rec
                . rec = { x = 3 }
                . mult =
                  | { x = x, y = y } -> x * y
                """
                        |> Expect.err
            , test "match record with constant" <|
                \_ ->
                    eval """
                mult rec
                . rec = { x = 4, y = 5 }
                . mult =
                  | { x = 3, y = y } -> 1
                  | { x = 4, y = y } -> 2
                """
                        |> Expect.equal (Ok (Int 2))
            , test "match record with non record fails" <|
                \_ ->
                    eval """
                get_x 3
                . get_x =
                  | { x = x } -> x
                """
                        |> Expect.err
            , test "match record doubly binds vars" <|
                \_ ->
                    eval """
                get_x rec
                . rec = { a = 3, b = 3 }
                . get_x =
                  | { a = x, b = x } -> x
                """
                        |> Expect.equal (Ok (Int 3))
            , test "match list binds vars" <|
                \_ ->
                    eval """
                mult xs
                . xs = [1, 2, 3, 4, 5]
                . mult =
                  | [1, x, 3, y, 5] -> x * y
                """
                        |> Expect.equal (Ok (Int 8))
            , test "match list incorrect length does not match" <|
                \_ ->
                    eval """
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
                    eval """
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
                    eval """
                get_x 3
                . get_x =
                  | [2, x] -> x
                """
                        |> Expect.err
            , test "match list doubly binds vars" <|
                \_ ->
                    eval """
                mult xs
                . xs = [1, 2, 3, 2, 1]
                . mult =
                  | [1, x, 3, x, 1] -> x
                """
                        |> Expect.equal (Ok (Int 2))
            , test "function can call itself" <|
                \_ ->
                    eval """
                f 1
                . f = n -> f n
                """
                        |> Expect.err
            , test "match function can call itself" <|
                \_ ->
                    eval """
                fac 5
                . fac =
                  | 0 -> 1
                  | 1 -> 1
                  | n -> n * fac (n - 1)
                """
                        |> Expect.equal (Ok (Int 120))
            , test "match list spread" <|
                \_ ->
                    eval """
                f [2, 4, 6]
                . f =
                  | [] -> 0
                  | [x, ...] -> x
                  | c -> 1
                """
                        |> Expect.equal (Ok (Int 2))
            , test "match list named spread" <|
                \_ ->
                    eval """
                tail [1,2,3]
                . tail =
                  | [first, ...rest] -> rest
                """
                        |> Expect.equal (Ok (List [ Int 2, Int 3 ]))
            , test "match record spread" <|
                \_ ->
                    eval """
                f {x = 4, y = 5}
                . f =
                  | {} -> 0
                  | {x = a, ...} -> a
                  | c -> 1
                """
                        |> Expect.equal (Ok (Int 4))
            , test "match expr as boolean variants" <|
                \_ ->
                    eval """
                say (1 < 2)
                . say =
                  | #false () -> "oh no"
                  | #true () -> "omg"
                """
                        |> Expect.equal (Ok (String "omg"))
            , test "match variant record" <|
                \_ ->
                    eval """
                f #add {x = 3, y = 4}
                . f =
                  | # b () -> "foo"
                  | #add {x = x, y = y} -> x + y
                """
                        |> Expect.equal (Ok (Int 7))
            , test "multiple where clauses" <|
                \_ ->
                    eval """
                x + y + z
                . x = 1
                . y = 2
                . z = 3
                """
                        |> Expect.equal (Ok (Int 6))
            , test "nested functions" <|
                \_ ->
                    eval """
                outer 5
                . outer = x ->
                    inner (x + 1)
                    . inner = y -> y * 2
                """
                        |> Expect.equal (Ok (Int 12))
            , test "higher order functions" <|
                \_ ->
                    eval """
                apply (x -> x * 2) 3
                . apply = f -> x -> f x
                """
                        |> Expect.equal (Ok (Int 6))
            , test "currying" <|
                \_ ->
                    eval """
                add 2 3
                . add = x -> y -> x + y
                """
                        |> Expect.equal (Ok (Int 5))
            , test "complex list operations" <|
                \_ ->
                    eval """
                map (x -> x * 2) [1, 2, 3]
                . map = f ->
                  | [] -> []
                  | [x, ...xs] -> f x >+ map f xs
                """
                        |> Expect.equal (Ok (List [ Int 2, Int 4, Int 6 ]))
            , test "record update" <|
                \_ ->
                    eval """
                update_x {x = 1, y = 2} 3
                . update-x = r -> new_x -> {x = new_x, ...r}
                """
                        |> Expect.equal (Ok (Record (Dict.fromList [ ( "x", Int 3 ), ( "y", Int 2 ) ])))

            {-
               , test "function can reference itself" <|
                   \_ ->
                       eval """
                       f 1
                       . f = n -> f
                       """
                           |> Expect.equal (Ok (Closure (Dict.singleton "f" (Var "f")) (Function (Var "n") (Var "f"))))
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
