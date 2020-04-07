(* Scanner tests *)

let test_scanner_basic () =
  let open Lox.Scanner in
  Alcotest.(check (list string))
    "same lists"
    [ "var"; "i"; ";"; "" ]
    (make_scanner "var i;" |> scan_tokens |> List.map (fun t -> t.lexeme))
;;

let test_scanner_advanced () =
  let open Lox.Scanner in
  Alcotest.(check (list string))
    "same lists"
    [ "var"
    ; "j"
    ; ";"
    ; "for"
    ; "("
    ; "var"
    ; "i"
    ; "="
    ; "0"
    ; ";"
    ; "i"
    ; "<"
    ; "5"
    ; ";"
    ; "i"
    ; "="
    ; "i"
    ; "+"
    ; "1"
    ; ")"
    ; "{"
    ; "if"
    ; "("
    ; "i"
    ; "<"
    ; "2"
    ; ")"
    ; "print"
    ; "i"
    ; ";"
    ; "}"
    ; ""
    ]
    (make_scanner "var j; for (var i = 0; i < 5; i = i + 1) { if (i < 2) print i; }"
    |> scan_tokens
    |> List.map (fun t -> t.lexeme))
;;

(* Parser tests *)

let test_parser_while_loop () =
  let open Lox in
  Alcotest.(check (list string))
    "same lists"
    [ "var i = 1;"; "while (i < 5) print i;" ]
    (Scanner.make_scanner "var i = 1; while (i < 5) print i;"
    |> Scanner.scan_tokens
    |> Parser.make_parser
    |> Parser.parse
    |> List.map Parser.string_of_statement)
;;

let test_parser_for_loop_simple () =
  let open Lox in
  Alcotest.(check (list string))
    "same lists"
    (* only one item in list because the for loop gets de-sugared into a block *)
    [ "{var i = 0; while (i < 5) {print i; i = (i + 1);}}" ]
    (Scanner.make_scanner "for (var i = 0; i < 5; i = i + 1) { print i; }"
    |> Scanner.scan_tokens
    |> Parser.make_parser
    |> Parser.parse
    |> List.map Parser.string_of_statement)
;;

let test_parser_for_loop_no_declaration_increment () =
  let open Lox in
  Alcotest.(check (list string))
    "same lists"
    [ "var i = 1;"; "while (i < 5) {print i; i = (i + 1);}" ]
    (Scanner.make_scanner "var i = 1; for (; i < 5; ) { print i; i = i + 1; }"
    |> Scanner.scan_tokens
    |> Parser.make_parser
    |> Parser.parse
    |> List.map Parser.string_of_statement)
;;

let test_parser_for_loop_infinite () =
  let open Lox in
  Alcotest.(check (list string))
    "same lists"
    [ "while (true) print (1 == 1);" ]
    (Scanner.make_scanner "for (;;) print 1 == 1;"
    |> Scanner.scan_tokens
    |> Parser.make_parser
    |> Parser.parse
    |> List.map Parser.string_of_statement)
;;

let test_parser_function_no_args () =
  let open Lox in
  Alcotest.(check (list string))
    "same lists"
    [ "test();" ]
    (Scanner.make_scanner "test();"
    |> Scanner.scan_tokens
    |> Parser.make_parser
    |> Parser.parse
    |> List.map Parser.string_of_statement)
;;

let test_parser_function_multiple_args () =
  let open Lox in
  Alcotest.(check (list string))
    "same lists"
    [ "test(v, (1 + 1), 5);" ]
    (Scanner.make_scanner "test(v, 1 + 1, 5);"
    |> Scanner.scan_tokens
    |> Parser.make_parser
    |> Parser.parse
    |> List.map Parser.string_of_statement)
;;

let test_parser_function_declaration_no_params () =
  let open Lox in
  Alcotest.(check (list string))
    "same lists"
    [ "fun test() {do_something();}" ]
    (Scanner.make_scanner "fun test() { do_something(); }"
    |> Scanner.scan_tokens
    |> Parser.make_parser
    |> Parser.parse
    |> List.map Parser.string_of_statement)
;;

let test_parser_function_declaration_multiple_params () =
  let open Lox in
  Alcotest.(check (list string))
    "same lists"
    [ "fun test(v1, v2, v3) {print v1; print v2; print (v1 + v3);}" ]
    (Scanner.make_scanner "fun test(v1, v2, v3) { print v1; print v2; print v1 + v3; }"
    |> Scanner.scan_tokens
    |> Parser.make_parser
    |> Parser.parse
    |> List.map Parser.string_of_statement)
;;

(* Interpreter tests *)

(* Utility function to execute a Unix command *)
(* Source: https://rosettacode.org/wiki/Execute_a_system_command#OCaml *)
let string_of_unix_command cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with
  | End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  Buffer.contents buf |> String.trim
;;

(* NOTE I can't figure out how to capture stdout into a string, so instead I read
 * from files and compare the results of running the lox file with the expected
 * file. Alcotest tests execute in the directory $PROJECT_ROOT/_build/default/test *)
let test_interpreter dir_name test_name _ =
  Alcotest.(check string)
    "same string"
    (Lox.read_file
    @@ Printf.sprintf "../../../test/test_files/%s/%s.expected" dir_name test_name)
    (string_of_unix_command
    @@ Printf.sprintf
         "../bin/main.exe ../../../test/test_files/%s/%s.lox"
         dir_name
         test_name)
;;

(* Utility function to make a list of test cases
 * `test_group` is a list of strings containing test filenames *)
let rec make_unit_tests name = function
  | [] -> []
  | filename :: filenames ->
    [ Alcotest.test_case ("test " ^ filename) `Slow (test_interpreter name filename) ]
    @ make_unit_tests name filenames
;;

(* Run tests *)
let () =
  Alcotest.run
    "Lox tests"
    [ ( "scanner tests"
      , [ Alcotest.test_case "simple scanner" `Quick test_scanner_basic
        ; Alcotest.test_case "advanced scanner" `Quick test_scanner_advanced
        ] )
    ; ( "parser tests"
      , [ Alcotest.test_case "parse while loops" `Quick test_parser_while_loop
        ; Alcotest.test_case "parse simple for loops" `Quick test_parser_for_loop_simple
        ; Alcotest.test_case
            "parse for loops without declaration and increment"
            `Quick
            test_parser_for_loop_no_declaration_increment
        ; Alcotest.test_case
            "parse infinite for loops"
            `Quick
            test_parser_for_loop_infinite
        ; Alcotest.test_case
            "parse function with no arguments"
            `Quick
            test_parser_function_no_args
        ; Alcotest.test_case
            "parse function with multiple arguments"
            `Quick
            test_parser_function_multiple_args
        ; Alcotest.test_case
            "parse function declaration with no parameters"
            `Quick
            test_parser_function_declaration_no_params
        ; Alcotest.test_case
            "parse function declaration with multiple parameters"
            `Quick
            test_parser_function_declaration_multiple_params
        ] )
    ; ( "interpreter tests"
      , [ Alcotest.test_case
            "interpret recursive function"
            `Slow
            (test_interpreter "_other" "test_fibonacci")
        ; Alcotest.test_case
            "interpret program with global variables"
            `Slow
            (test_interpreter "_other" "test_globals")
        ; Alcotest.test_case
            "interpret simple closure"
            `Slow
            (test_interpreter "_other" "test_simple_closure")
        ; Alcotest.test_case
            "interpret closure with globals"
            `Slow
            (test_interpreter "_other" "test_closure_globals")
        ] )
    ; ( "assignment tests"
      , make_unit_tests "assignment" [ "syntax"; "global"; "associativity"; "local" ] )
    ; ( "closure test"
      , make_unit_tests
          "closure"
          [ "reuse_closure_slot"
          ; "assign_to_shadowed_later"
          ; "close_over_later_variable"
          ; "closed_closure_in_function"
          ; "unused_later_closure"
          ; "shadow_closure_with_local"
          ; "unused_closure"
          ; "close_over_function_parameter"
          ; "close_over_method_parameter"
          ; "open_closure_in_function"
          ; "reference_closure_multiple_times"
          ; "nested_closure"
          ; "assign_to_closure"
          ] )
    ; "comments test", make_unit_tests "comments" [ "line_at_eof"; "unicode" ]
    ; ( "variable tests"
      , make_unit_tests
          "variable"
          [ "in_nested_block"
          ; "scope_reuse_in_different_blocks"
          ; "use_global_in_initializer"
          ; "redeclare_global"
          ; "shadow_and_local"
          ; "early_bound"
          ; "uninitialized"
          ; "shadow_global"
          ; "in_middle_of_block"
          ; "shadow_local"
          ; "unreached_undefined"
          ; "redefine_global"
          ] )
    ; "nil tests", make_unit_tests "nil" [ "literal" ]
    ; "if tests", make_unit_tests "if" [ "dangling_else"; "truth"; "else"; "if" ]
    ; ( "return tests"
      , make_unit_tests
          "return"
          [ "after_if"
          ; "after_else"
          ; "return_nil_if_no_value"
          ; "in_function"
          ; "after_while"
          ] )
    ; ( "function tests"
      , make_unit_tests
          "function"
          [ "empty_body"
          ; "parameters"
          ; "local_recursion"
          ; "recursion"
          ; "print"
          ; "mutual_recursion"
          ] )
    ; ( "scanning tests"
      , make_unit_tests
          "scanning"
          [ "numbers"; "keywords"; "punctuators"; "whitespace"; "identifiers"; "strings" ]
      )
    ; "number tests", make_unit_tests "number" [ "nan_equality"; "literals" ]
    ; ( "logical operators tests"
      , make_unit_tests "logical_operator" [ "and"; "or"; "and_truth"; "or_truth" ] )
    ; "boolean tests", make_unit_tests "bool" [ "equality"; "not" ]
    ; "expressions tests", make_unit_tests "expressions" [ "evaluate"; "parse" ]
      (* ; ( "for loop tests"
       *   , make_unit_tests
       *       "for"
       *       [ "return_closure"; "scope"; "syntax"; "return_inside"; "closure_in_body" ] ) *)
    ; "string tests", make_unit_tests "string" [ "literals"; "multiline" ]
      (* ; ( "while loop tests"
       *   , make_unit_tests
       *       "while"
       *       [ "return_closure"; "syntax"; "return_inside"; "closure_in_body" ] ) *)
    ; ( "operator tests"
      , make_unit_tests
          "operator"
          [ "multiply"
          ; "negate"
          ; "comparison"
          ; "not_equals"
          ; "add"
          ; "equals"
          ; "divide"
          ; "not"
          ; "subtract"
          ] )
    ; "block tests", make_unit_tests "block" [ "empty"; "scope" ]
    ]
;;
