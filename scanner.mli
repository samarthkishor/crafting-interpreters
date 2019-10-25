module Error : sig
  type runtime_error = {
    where: int;
    message: string
  }
  exception RuntimeError of runtime_error
  val had_error: bool ref
  val report: int -> string -> string -> unit
  val error: int -> string -> unit
end

module Value : sig
  type t =
    | LoxBool of bool
    | LoxInt of int
    | LoxNumber of float
    | LoxNil
end

type token_type =
  (* Single character tokens *)
  | LeftParen | RightParen | LeftBrace | RightBrace
  | Comma | Dot | Minus | Plus | Semicolon | Slash | Star
  (* One or two character tokens *)
  | Bang | BangEqual
  | Equal | EqualEqual
  | Greater | GreaterEqual
  | Less | LessEqual
  (* Literals *)
  | Identifier | String | Number
  (* Keywords *)
  | And | Class | Else | False | Fun | For | If | Nil | Or | Print | Return
  | Super | This | True | Var | While
  | Eof

type token = {
  token_type : token_type;
  lexeme : string;
  literal : Value.t;
  line : int;
}

type scanner = {
  source: string;
  tokens: token list;
  start: int;
  current: int;
  line: int;
}

val make_scanner : string -> scanner

val is_at_end : scanner -> bool

val advance_scanner : scanner -> scanner

val get_char : scanner -> char option

val add_token : scanner -> token_type -> scanner

val add_double_token : scanner -> token_type -> token_type -> scanner

val scan_token : scanner -> scanner

val scan_tokens : scanner -> token list
