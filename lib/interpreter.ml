type t = { environment : Environment.t }

exception Return of Value.t

let is_truthy value =
  match value with
  | Value.LoxNil -> false
  | Value.LoxBool b -> b
  | _ -> true
;;

let is_equal left right =
  let open Value in
  match left, right with
  | LoxNil, LoxNil -> true
  | LoxNil, _ -> false
  | l, r -> l = r
;;

let binary_arithmetic operator left right =
  let open Value in
  match left, right with
  | LoxNumber l, LoxNumber r -> LoxNumber (operator l r)
  | LoxNumber _, v ->
    raise @@ Error.TypeError { observed_type = type_of v; expected_type = Number }
  | v, LoxNumber _ ->
    raise @@ Error.TypeError { observed_type = type_of v; expected_type = Number }
  | v, _ -> raise @@ Error.TypeError { observed_type = type_of v; expected_type = Number }
;;

let binary_comparison operator left right =
  let open Value in
  match left, right with
  | LoxNumber l, LoxNumber r -> LoxBool (operator l r)
  | LoxNumber _, v ->
    raise @@ Error.TypeError { observed_type = type_of v; expected_type = Number }
  | v, LoxNumber _ ->
    raise @@ Error.TypeError { observed_type = type_of v; expected_type = Number }
  | v, _ -> raise @@ Error.TypeError { observed_type = type_of v; expected_type = Number }
;;

let rec evaluate environment (expr : Parser.expr) =
  match expr with
  | Literal e -> e.value
  | Grouping e -> evaluate environment e.expression
  | Unary e ->
    let right = evaluate environment e.operand in
    (match e.unary_operator.token_type with
    | Minus ->
      LoxNumber
        (match right with
        | LoxNumber n -> -.n
        | value ->
          raise
          @@ Error.TypeError
               { observed_type = Value.type_of value; expected_type = Number })
    | Bang -> LoxBool (not (is_truthy right))
    | _ -> LoxNil)
  | Binary e ->
    let left = evaluate environment e.left in
    let right = evaluate environment e.right in
    (match e.binary_operator.token_type with
    | Minus -> binary_arithmetic ( -. ) left right
    | Slash -> binary_arithmetic ( /. ) left right
    | Star -> binary_arithmetic ( *. ) left right
    | Plus ->
      (match left, right with
      | LoxNumber _, LoxNumber _ -> binary_arithmetic ( +. ) left right
      | LoxString l, LoxString r -> LoxString (l ^ r)
      | LoxString _, v | v, LoxString _ ->
        raise
        @@ Error.TypeError { observed_type = Value.type_of v; expected_type = String }
      | v, _ ->
        raise
        @@ Error.TypeError { observed_type = Value.type_of v; expected_type = String })
    | Greater -> binary_comparison ( > ) left right
    | GreaterEqual -> binary_comparison ( >= ) left right
    | Less -> binary_comparison ( < ) left right
    | LessEqual -> binary_comparison ( <= ) left right
    | BangEqual -> LoxBool (not (is_equal left right))
    | EqualEqual -> LoxBool (is_equal left right)
    | _ -> LoxNil)
  | Call c ->
    let callee = evaluate environment c.callee in
    let evaluated_args = List.map (fun arg -> evaluate environment arg) c.arguments in
    (match callee with
    | Value.LoxFunction f ->
      if List.length evaluated_args = f.arity
      then Value.call callee evaluated_args
      else
        raise
        @@ Error.RuntimeError
             { where = c.paren.line
             ; message =
                 Printf.sprintf
                   "Expected %d arguments but got %d."
                   f.arity
                   (List.length evaluated_args)
             }
    | _ ->
      raise
      @@ Error.RuntimeError
           { where = c.paren.line; message = "Can only call functions and classes." })
  | Variable token -> Environment.get_value environment token
  | Assign expr ->
    let value = evaluate environment expr.assign_value in
    Environment.assign environment expr.name value
  | Logical expr ->
    let left = evaluate environment expr.logical_left in
    (* short-circuit `or` if left evaluates to true *)
    if expr.operator.token_type = Scanner.Or
    then
      if is_truthy left
      then left
      else
        evaluate environment expr.logical_right
        (* short-circuit `and` if left evaluates to false *)
    else if not (is_truthy left)
    then left
    else evaluate environment expr.logical_right
;;

let rec evaluate_statement (environment : Environment.t) (statement : Parser.statement) =
  match statement with
  | Expression expression -> ignore (evaluate environment expression)
  | IfStatement s ->
    if is_truthy (evaluate environment s.condition)
    then evaluate_statement environment s.then_branch
    else (
      match s.else_branch with
      | None -> ()
      | Some branch -> evaluate_statement environment branch)
  | Print expression ->
    Environment.print_environment environment;
    evaluate environment expression |> Value.string_of |> Printf.printf "%s\n"
  | ReturnStatement s ->
    let return_value =
      match s.value with
      | None -> Value.LoxNil
      | Some e -> evaluate environment e
    in
    raise @@ Return return_value
  | FunctionDeclaration f ->
    (* referenced  https://github.com/isaacazuelos/crafting-interpreters-ocaml/blob/master/interpreter.ml#L107 *)
    let env = Environment.init ~enclosing:environment () in
    let func_state : Environment.t =
      { values = environment.values; enclosing = Some env }
    in
    let call_func args =
      let new_env =
        match func_state.enclosing with
        | None -> Environment.init ()
        | Some enclosing -> Environment.init ~enclosing ()
      in
      List.iter2
        (fun (param : Scanner.token) arg -> Environment.define new_env param.lexeme arg)
        f.params
        args;
      try
        interpret ~environment:new_env f.fun_body;
        Value.LoxNil
      with
      | Return value -> value
    in
    Environment.define
      func_state
      f.fun_name.lexeme
      (LoxFunction { arity = List.length f.params; callable = call_func })
  | VarDeclaration d ->
    let value =
      match d.init with
      | Literal l ->
        if l.value = LoxNil then Value.LoxNil else evaluate environment d.init
      | _ -> evaluate environment d.init
    in
    Environment.define environment d.name.lexeme value
  | WhileStatement s ->
    while is_truthy (evaluate environment s.while_condition) do
      evaluate_statement environment s.body
    done
  | Block block_statements ->
    let previous_environment = environment in
    let new_environment = Environment.init ~enclosing:previous_environment () in
    (try
       interpret ~environment:new_environment block_statements
       (* restore the previous environment even if there was an error *)
     with
    | error ->
      environment.enclosing <- Some previous_environment;
      raise error);
    environment.enclosing <- Some previous_environment

and interpret ?(environment = Environment.init ()) (statements : Parser.statement list) =
  try
    List.iter (fun statement -> evaluate_statement environment statement) statements
  with
  | Error.TypeError error -> Error.report_runtime_error (Error.TypeError error)
  | Error.RuntimeError error -> Error.report_runtime_error (Error.RuntimeError error)
;;
