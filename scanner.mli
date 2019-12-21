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

type token =
  { token_type : token_type
  ; lexeme : string
  ; literal : Value.t
  ; line : int }

type scanner =
  { source : string
  ; tokens : token list
  ; start : int
  ; current : int
  ; line : int }

val make_scanner : string -> scanner
val scan_tokens : scanner -> token list
