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
        ] )
    ]
;;
