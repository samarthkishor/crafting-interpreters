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
let test_interpreter test_name _ =
  Alcotest.(check string)
    "same string"
    (Lox.read_file @@ Printf.sprintf "../../../test/test_files/test_%s.expected" test_name)
    (string_of_unix_command
    @@ Printf.sprintf "../bin/main.exe ../../../test/test_files/test_%s.lox" test_name)
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
            (test_interpreter "fibonacci")
        ; Alcotest.test_case
            "interpret program with global variables"
            `Slow
            (test_interpreter "globals")
        ; Alcotest.test_case
            "interpret simple closure"
            `Slow
            (test_interpreter "simple_closure")
        ; Alcotest.test_case
            "interpret closure with globals"
            `Slow
            (test_interpreter "closure_globals")
        ] )
    ]
;;
