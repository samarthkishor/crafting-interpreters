(* TODO Refactor this eventually to be purely functional *)
(* TODO Fix parser error handling *)

type expr =
  | Literal of literal
  | Unary of unary
  | Binary of binary
  | Call of call
  | Grouping of grouping
  | Variable of Scanner.token
  | Assign of assign
  | Logical of logical

and unary =
  { unary_operator : Scanner.token
  ; operand : expr
  }

and binary =
  { left : expr
  ; binary_operator : Scanner.token
  ; right : expr
  }

and call =
  { callee : expr
  ; paren : Scanner.token
  ; arguments : expr list
  }

and grouping = { expression : expr }

and literal =
  { token : Scanner.token
  ; value : Value.t
  }

and assign =
  { name : Scanner.token
  ; assign_value : expr
  }

and logical =
  { logical_left : expr
  ; operator : Scanner.token
  ; logical_right : expr
  }

type parser =
  { tokens : Scanner.token array
  ; mutable current : int
  }

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
  | Call c ->
    string_of_expr c.callee
    ^ "("
    ^ String.concat ", " (List.map (fun arg -> string_of_expr arg) c.arguments)
    ^ ")"
  | Grouping e -> "(" ^ string_of_expr e.expression ^ ")"
  | Literal e -> Value.string_of e.value
  | Unary e -> "(" ^ e.unary_operator.lexeme ^ " " ^ string_of_expr e.operand ^ ")"
  | Variable e -> e.lexeme
  | Assign e -> e.name.lexeme ^ " = " ^ string_of_expr e.assign_value
  | Logical e ->
    string_of_expr e.logical_left
    ^ " "
    ^ e.operator.lexeme
    ^ " "
    ^ string_of_expr e.logical_right
;;

type statement =
  | Expression of expr
  | IfStatement of if_statement
  | Print of expr
  | VarDeclaration of var_declaration
  | WhileStatement of while_statement
  | Block of statement list

and if_statement =
  { condition : expr
  ; then_branch : statement
  ; else_branch : statement option
  }

and var_declaration =
  { name : Scanner.token
  ; init : expr
  }

and while_statement =
  { while_condition : expr
  ; body : statement
  }

let rec string_of_statement stmt =
  match stmt with
  | Expression e -> string_of_expr e ^ ";"
  | IfStatement s ->
    "if ("
    ^ string_of_expr s.condition
    ^ ") "
    ^ "{ "
    ^ string_of_statement s.then_branch
    ^ "} "
    ^
    (match s.else_branch with
    | None -> ""
    | Some b -> " else { " ^ string_of_statement b ^ "}")
  | Print e -> "print " ^ string_of_expr e ^ ";"
  | VarDeclaration e ->
    let right_side =
      match e.init with
      | Literal l -> if l.value = LoxNil then ";" else " = " ^ string_of_expr e.init ^ ";"
      | _ -> " = " ^ string_of_expr e.init ^ ";"
    in
    "var " ^ e.name.lexeme ^ right_side
  | Block statements ->
    "{" ^ (List.map string_of_statement statements |> String.concat " ") ^ "}"
  | WhileStatement s ->
    (* make sure there are always parentheses around the while condition *)
    let while_condition =
      match s.while_condition with
      | Literal _ -> "(" ^ string_of_expr s.while_condition ^ ")"
      | _ -> string_of_expr s.while_condition
    in
    "while " ^ while_condition ^ " " ^ string_of_statement s.body
;;

(* For debugging *)
let print_statements statements =
  List.iter (fun s -> string_of_statement s |> Printf.printf "%s\n") statements
;;

let make_parser tokens = { tokens = Array.of_list tokens; current = 0 }
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
      true)
    else matches parser ts
;;

let consume parser token_type message =
  if check parser token_type
  then advance parser
  else
    raise
      (Error.ParseError
         { line = (peek parser).line; lexeme = (peek parser).lexeme; message })
;;

let synchronize parser =
  ignore (advance parser);
  while not (at_end parser) do
    if (previous parser).token_type = Semicolon
    then ()
    else (
      match (peek parser).token_type with
      | Class | Fun | Var | For | If | While | Print | Return -> ()
      | _ -> ignore (advance parser))
  done;
  parser
;;

(* Parses left associative binary expressions *)
let rec binary next_precedence token_types parser =
  let expr = ref (next_precedence parser) in
  while matches parser token_types do
    let operator = previous parser in
    let right = next_precedence parser in
    expr := Binary { left = !expr; binary_operator = operator; right }
  done;
  !expr

(* Parses logical and and or *)
and logical next_precedence token_type parser =
  let expr = ref (next_precedence parser) in
  while matches parser [ token_type ] do
    let operator = previous parser in
    let logical_right = next_precedence parser in
    expr := Logical { logical_left = !expr; operator; logical_right }
  done;
  !expr

(* Rule: primary → NUMBER | STRING | "false" | "true" | "nil"
                   | "(" expression ")"
                   | IDENTIFIER *)
and primary parser =
  let token = advance parser in
  match token.token_type with
  | False -> Literal { token; value = LoxBool false }
  | True -> Literal { token; value = LoxBool true }
  | Nil -> Literal { token; value = LoxNil }
  | Number -> Literal { token; value = LoxNumber (float_of_string token.lexeme) }
  | String -> Literal { token; value = LoxString token.lexeme }
  | Identifier -> Variable token
  | LeftParen ->
    let expr = expression parser in
    ignore (consume parser RightParen "Expect ')' after expression.");
    Grouping { expression = expr }
  | _ ->
    raise
      (Error.ParseError
         { line = token.line; lexeme = token.lexeme; message = "Expect expression." })

(* Rule: expression -> assignment *)
and expression parser = assignment parser

(* Rule: assignment -> IDENTIFIER '=' assignment | logical_or *)
and assignment parser =
  let expr = logical_or parser in
  if matches parser [ Equal ]
  then (
    let equals = previous parser in
    (* assignment is right-associative *)
    let value = assignment parser in
    match expr with
    | Variable name -> Assign { name; assign_value = value }
    | _ ->
      raise
        (Error.ParseError
           { line = equals.line
           ; lexeme = equals.lexeme
           ; message = "Invalid assignment target."
           }))
  else expr

(* Rule: logical_or -> logical_and ( "or" logic_and )* *)
and logical_or parser = logical logical_and Or parser
(* Rule: logical_and  → equality ( "and" equality )* *)
and logical_and parser = logical equality And parser
(* Rule: equality -> comparison ( ( "!=" | "==" ) comparison )* *)
and equality parser = binary comparison [ BangEqual; EqualEqual ] parser
(* Rule: comparison -> addition ( ( ">" | ">=" | "<" | "<=" ) addition )* *)
and comparison parser = binary addition [ Greater; GreaterEqual; Less; LessEqual ] parser
(* Rule: addition → multiplication ( ( "-" | "+" ) multiplication )* *)
and addition parser = binary multiplication [ Minus; Plus ] parser
(* Rule: multiplication → unary ( ( "/" | "*" ) unary )* *)
and multiplication parser = binary unary [ Slash; Star ] parser

(* Rule: unary → ( "!" | "-" ) unary | call *)
and unary parser =
  if matches parser [ Bang; Minus ]
  then (
    let operator = previous parser in
    let operand = unary parser in
    Unary { unary_operator = operator; operand })
  else call parser

(* Rule: call → primary ( "(" arguments? ")" )*
 *       arguments → expression ( "," expression )* *)
and call parser =
  let finish_call callee =
    let rec add_args args =
      if not (matches parser [ Comma ])
      then List.rev args
      else add_args (expression parser :: args)
    in
    let arguments =
      if not (check parser RightParen) then add_args [ expression parser ] else []
    in
    (* warn if there are 255+ arguments to a function *)
    let () =
      if List.length arguments >= 255
      then Error.error (peek parser).line "Function cannot have more than 255 arguments."
    in
    let paren = consume parser RightParen "Expect ')' after arguments." in
    Call { callee; paren; arguments }
  in
  let rec loop expr =
    if not (matches parser [ LeftParen ]) then expr else loop (finish_call expr)
  in
  loop (primary parser)
;;

(* Rule: statement → exprStmt | forStmt | ifStmt | printStmt | whileStmt | block *)
let rec statement parser =
  (* TODO refactor this to use pattern-matching *)
  if matches parser [ For ]
  then for_statement parser
  else if matches parser [ If ]
  then if_statement parser
  else if matches parser [ Print ]
  then make_statement (fun e -> Print e) parser
  else if matches parser [ While ]
  then while_statement parser
  else if matches parser [ LeftBrace ]
  then Block (block parser)
  else make_statement (fun e -> Expression e) parser

and if_statement parser =
  let _ = consume parser LeftParen "Expect '(' after 'if'." in
  let condition = expression parser in
  let _ = consume parser RightParen "Expect ')' after 'if' condition." in
  let then_branch = statement parser in
  let else_branch = if matches parser [ Else ] then Some (statement parser) else None in
  IfStatement { condition; then_branch; else_branch }

and while_statement parser =
  let _ = consume parser LeftParen "Expect '(' after 'while'." in
  let while_condition = expression parser in
  let _ = consume parser RightParen "Expect ')' after condition." in
  let body = statement parser in
  WhileStatement { while_condition; body }

and for_statement parser =
  let _ = consume parser LeftParen "Expect '(' after 'for'." in
  let initial =
    if matches parser [ Semicolon ]
    then None
    else if matches parser [ Var ]
    then Some (var_declaration parser)
    else Some (make_statement (fun e -> Expression e) parser)
  in
  let condition =
    if not (check parser Semicolon) then Some (expression parser) else None
  in
  let _ = consume parser Semicolon "Expect ';' after loop condition." in
  let increment =
    if not (check parser RightParen) then Some (expression parser) else None
  in
  let _ = consume parser RightParen "Expect ')' after for clauses." in
  let temp_body = statement parser in
  let body =
    let true_cond =
      Literal
        { token =
            { token_type = True
            ; lexeme = "true"
            ; literal = LoxBool true
            ; line = (peek parser).line
            }
        ; value = LoxBool true
        }
    in
    match increment, condition, initial with
    | None, None, None -> WhileStatement { while_condition = true_cond; body = temp_body }
    | None, Some cond, None -> WhileStatement { while_condition = cond; body = temp_body }
    | None, None, Some init ->
      Block [ init; WhileStatement { while_condition = true_cond; body = temp_body } ]
    | None, Some cond, Some init ->
      Block [ init; WhileStatement { while_condition = cond; body = temp_body } ]
    | Some inc, None, None ->
      let block =
        match temp_body with
        | Block b ->
          let new_block = b @ [ Expression inc ] in
          Block new_block
        | _ -> Block [ temp_body; Expression inc ]
      in
      WhileStatement { while_condition = true_cond; body = block }
    | Some inc, Some cond, None ->
      let block =
        match temp_body with
        | Block b ->
          let new_block = b @ [ Expression inc ] in
          Block new_block
        | _ -> Block [ temp_body; Expression inc ]
      in
      WhileStatement { while_condition = cond; body = block }
    | Some inc, None, Some init ->
      let block =
        match temp_body with
        | Block b ->
          let new_block = b @ [ Expression inc ] in
          Block new_block
        | _ -> Block [ temp_body; Expression inc ]
      in
      Block [ init; WhileStatement { while_condition = true_cond; body = block } ]
    | Some inc, Some cond, Some init ->
      let block =
        match temp_body with
        | Block b ->
          let new_block = b @ [ Expression inc ] in
          Block new_block
        | _ -> Block [ temp_body; Expression inc ]
      in
      Block [ init; WhileStatement { while_condition = cond; body = block } ]
  in
  body

and make_statement (statement_type : expr -> statement) parser =
  let expr = expression parser in
  ignore (consume parser Semicolon "Expect ';' after statement.");
  statement_type expr

and var_declaration parser =
  let name = consume parser Identifier "Expect variable name." in
  let init =
    if matches parser [ Equal ]
    then expression parser
    else Literal { token = peek parser; value = LoxNil }
  in
  ignore (consume parser Semicolon "Expect ';' after variable declaration");
  VarDeclaration { name; init }

and declaration parser =
  try if matches parser [ Var ] then var_declaration parser else statement parser with
  | Error.ParseError error ->
    Error.report_parse_error error;
    statement (synchronize parser)

and block ?(statements = []) parser =
  (* avoid infinite loops if a '}' is missing by adding the at_end check *)
  if at_end parser || check parser RightBrace
  then (
    let _ = consume parser RightBrace "Expect '}' after block." in
    List.rev statements)
  else block ~statements:(declaration parser :: statements) parser
;;

(* Parses the expression *)
let rec parse ?(statements = []) parser =
  if at_end parser
  then List.rev statements
  else parse ~statements:(declaration parser :: statements) parser
;;
