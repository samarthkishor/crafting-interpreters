(* TODO Refactor this eventually to be purely functional *)
(* based off https://github.com/isaacazuelos/crafting-interpreters-ocaml/blob/master/parser.ml *)

type expr =
  | Literal of literal
  | Unary of unary
  | Binary of binary
  | Grouping of grouping

and unary =
  { unary_operator : Scanner.token
  ; operand : expr }

and binary =
  { left : expr
  ; binary_operator : Scanner.token
  ; right : expr }

and grouping = {expression : expr}

and literal =
  { token : Scanner.token
  ; value : Value.t }

type parser =
  { tokens : Scanner.token array
  ; mutable current : int }

let rec string_of_expr expr =
  match expr with
  | Binary e ->
    "("
    ^ string_of_expr e.left
    ^ " "
    ^ e.binary_operator.lexeme
    ^ " "
    ^ string_of_expr e.right
    ^ ")"
  | Grouping e -> "(" ^ string_of_expr e.expression ^ ")"
  | Literal e -> Value.string_of e.value
  | Unary e -> "(" ^ e.unary_operator.lexeme ^ " " ^ string_of_expr e.operand ^ ")"
;;

let make_parser tokens = {tokens = Array.of_list tokens; current = 0}
let at_end parser = parser.current >= Array.length parser.tokens - 1
let previous parser = parser.tokens.(parser.current - 1)
let peek parser = parser.tokens.(parser.current)

let advance parser =
  if not (at_end parser) then parser.current <- parser.current + 1;
  previous parser
;;

let check parser token_type =
  if at_end parser then false else (peek parser).token_type == token_type
;;

let rec matches parser token_types =
  match token_types with
  | [] -> false
  | t :: ts ->
    if check parser t
    then (
      ignore (advance parser);
      true )
    else matches parser ts
;;

exception ParseError

let error (token : Scanner.token) message =
  if token.token_type = Eof
  then Error.report token.line " at end" message
  else Error.report token.line (" at '" ^ token.lexeme ^ "'") message;
  ParseError
;;

let consume parser token_type message =
  if check parser token_type then advance parser else raise (error (peek parser) message)
;;

let synchronize parser =
  ignore (advance parser);
  while not (at_end parser) do
    if (previous parser).token_type = Semicolon
    then ()
    else
      match (peek parser).token_type with
      | Class | Fun | Var | For | If | While | Print | Return -> ()
      | _ -> ignore (advance parser)
  done
;;

(* Parses left associative binary expressions *)
let rec binary next_precedence token_types parser =
  let expr = ref (next_precedence parser) in
  while matches parser token_types do
    let operator = previous parser in
    let right = next_precedence parser in
    expr := Binary {left = !expr; binary_operator = operator; right}
  done;
  !expr

(* Rule: primary → NUMBER | STRING | "false" | "true" | "nil"
                   | "(" expression ")" *)
and primary parser =
  let token = advance parser in
  match token.token_type with
  | False -> Literal {token; value = LoxBool false}
  | True -> Literal {token; value = LoxBool true}
  | Nil -> Literal {token; value = LoxNil}
  | Number -> Literal {token; value = LoxNumber (float_of_string token.lexeme)}
  | String -> Literal {token; value = LoxString token.lexeme}
  | LeftParen ->
    let expr = expression parser in
    ignore (consume parser RightParen "Expect ')' after expression.");
    Grouping {expression = expr}
  | _ -> raise (error token "Expect expression.")

(* Rule: expression -> equality *)
and expression parser = equality parser
(* Rule: equality -> comparison ( ( "!=" | "==" ) comparison )* *)
and equality parser = binary comparison [BangEqual; EqualEqual] parser
(* Rule: comparison -> addition ( ( ">" | ">=" | "<" | "<=" ) addition )* *)
and comparison parser = binary addition [Greater; GreaterEqual; Less; LessEqual] parser
(* Rule: addition → multiplication ( ( "-" | "+" ) multiplication )* *)
and addition parser = binary multiplication [Minus; Plus] parser
(* Rule: multiplication → unary ( ( "/" | "*" ) unary )* *)
and multiplication parser = binary unary [Slash; Star] parser

(* Rule: unary → ( "!" | "-" ) unary | primary *)
and unary parser =
  if matches parser [Bang; Minus]
  then
    let operator = previous parser in
    let operand = unary parser in
    Unary {unary_operator = operator; operand}
  else primary parser
;;

(* Parses the expression *)
let parse parser = expression parser