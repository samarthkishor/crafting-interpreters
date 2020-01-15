type token_type =
  (* Single character tokens *)
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  (* One or two character tokens *)
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  (* Literals *)
  | Identifier
  | String
  | Number
  (* Keywords *)
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | Eof
[@@deriving eq]

type token =
  { token_type : token_type
  ; lexeme : string
  ; literal : Value.t
  ; line : int
  }

type scanner =
  { source : string
  ; tokens : token list
  ; start : int
  ; current : int
  ; line : int
  }

let keywords =
  [ "and", And
  ; "class", Class
  ; "else", Else
  ; "false", False
  ; "fun", Fun
  ; "for", For
  ; "if", If
  ; "nil", Nil
  ; "or", Or
  ; "print", Print
  ; "return", Return
  ; "super", Super
  ; "this", This
  ; "true", True
  ; "var", Var
  ; "while", While
  ]
;;

let make_scanner source = { source; tokens = []; start = 0; current = 0; line = 1 }
let is_at_end scanner = scanner.current >= String.length scanner.source
let advance_scanner scanner = { scanner with current = scanner.current + 1 }

let get_char scanner =
  if scanner.current > String.length scanner.source
  then None
  else Some scanner.source.[scanner.current - 1]
;;

let get_lexeme scanner =
  String.sub scanner.source scanner.start (scanner.current - scanner.start)
;;

let add_token scanner token_type =
  let token =
    { token_type
    ; (* NOTE String.sub is weird in OCaml... see documentation *)
      lexeme = get_lexeme scanner
    ; literal = Value.LoxNil
    ; line = scanner.line
    }
  in
  { scanner with tokens = token :: scanner.tokens }
;;

let add_token_with_literal scanner token_type literal =
  let token = { token_type; lexeme = get_lexeme scanner; literal; line = scanner.line } in
  { scanner with tokens = token :: scanner.tokens }
;;

let add_double_token scanner double_token single_token =
  match scanner |> advance_scanner |> get_char with
  | None -> add_token scanner single_token
  | Some c ->
    if c <> '='
    then add_token scanner single_token
    else add_token (advance_scanner scanner) double_token
;;

let peek scanner =
  if scanner.current >= String.length scanner.source
  then '\x00'
  else scanner.source.[scanner.current]
;;

let add_comment scanner =
  match scanner |> advance_scanner |> get_char with
  | None -> add_token scanner Slash
  | Some c ->
    if c = '/'
    then (
      let rec comment_out scanner =
        if is_at_end scanner || peek scanner = '\n'
        then scanner
        else scanner |> advance_scanner |> comment_out
      in
      comment_out scanner)
    else add_token scanner Slash
;;

let rec consume_string scanner =
  if peek scanner = '"' && not (is_at_end scanner)
  then (
    let scanner = advance_scanner scanner in
    let literal =
      Value.LoxString
        (String.sub
           scanner.source
           (scanner.start + 1)
           (scanner.current - scanner.start - 1))
    in
    add_token_with_literal scanner String literal)
  else if is_at_end scanner
  then (
    LoxError.error scanner.line "Unterminated String.";
    scanner)
  else scanner |> advance_scanner |> consume_string
;;

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'z') || c == '_'
let is_alphanumeric c = is_digit c || is_alpha c

let rec number scanner =
  if not (is_digit (peek scanner) || peek scanner = '.')
  then (
    let value = get_lexeme scanner in
    let num =
      try Some (float_of_string value) with
      | _ -> None
    in
    match num with
    | None ->
      LoxError.error scanner.line "Invalid Number.";
      scanner
    | Some n -> add_token_with_literal scanner Number (Value.LoxNumber n))
  else scanner |> advance_scanner |> number
;;

let rec identifier scanner =
  if not (is_alphanumeric (peek scanner))
  then (
    let text = get_lexeme scanner in
    let token_type =
      match List.assoc_opt text keywords with
      | None -> Identifier
      | Some t -> t
    in
    add_token scanner token_type)
  else scanner |> advance_scanner |> identifier
;;

let scan_token scanner =
  let scanner = advance_scanner scanner in
  match get_char scanner with
  | None -> scanner
  | Some c ->
    (match c with
    | '(' -> add_token scanner LeftParen
    | ')' -> add_token scanner RightParen
    | '{' -> add_token scanner LeftBrace
    | '}' -> add_token scanner RightBrace
    | ',' -> add_token scanner Comma
    | '.' -> add_token scanner Dot
    | '-' -> add_token scanner Minus
    | '+' -> add_token scanner Plus
    | ';' -> add_token scanner Semicolon
    | '*' -> add_token scanner Star
    | '!' -> add_double_token scanner BangEqual Bang
    | '=' -> add_double_token scanner EqualEqual Equal
    | '<' -> add_double_token scanner LessEqual Less
    | '>' -> add_double_token scanner GreaterEqual Greater
    | '/' -> add_comment scanner
    | ' ' | '\r' | '\t' -> scanner
    | '\n' -> { scanner with line = scanner.line + 1 }
    | '"' -> consume_string scanner
    | c when is_digit c -> number scanner
    | c when is_alpha c -> identifier scanner
    | _ ->
      LoxError.error scanner.line "Unexpected Character.";
      scanner)
;;

let rec scan_tokens scanner =
  if is_at_end scanner
  then (
    let token =
      { token_type = Eof; lexeme = ""; literal = Value.LoxNil; line = scanner.line }
    in
    List.rev (token :: scanner.tokens))
  else (
    let scanner = { scanner with start = scanner.current } in
    scan_tokens (scan_token scanner))
;;
