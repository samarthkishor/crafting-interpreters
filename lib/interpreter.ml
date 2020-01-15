open Base

type t = { environment : Environment.t }

type state =
  { state_env : Environment.t
  ; globals : Environment.t
  }

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
  | l, r -> Value.equal_eval_type (Value.type_of l) (Value.type_of r)
;;

let binary_arithmetic operator left right =
  let open Value in
  match left, right with
  | LoxNumber l, LoxNumber r -> LoxNumber (operator l r)
  | LoxNumber _, v ->
    raise @@ LoxError.TypeError { observed_type = type_of v; expected_type = Number }
  | v, LoxNumber _ ->
    raise @@ LoxError.TypeError { observed_type = type_of v; expected_type = Number }
  | v, _ ->
    raise @@ LoxError.TypeError { observed_type = type_of v; expected_type = Number }
;;

let binary_comparison operator left right =
  let open Value in
  match left, right with
  | LoxNumber l, LoxNumber r -> LoxBool (operator l r)
  | LoxNumber _, v ->
    raise @@ LoxError.TypeError { observed_type = type_of v; expected_type = Number }
  | v, LoxNumber _ ->
    raise @@ LoxError.TypeError { observed_type = type_of v; expected_type = Number }
  | v, _ ->
    raise @@ LoxError.TypeError { observed_type = type_of v; expected_type = Number }
;;

let rec evaluate (state : state) (expr : Parser.expr) : state * Value.t =
  (* Environment.print_environment state.state_env; *)
  match expr with
  | Literal e -> state, e.value
  | Grouping e -> evaluate state e.expression
  | Unary e ->
    let new_state, right = evaluate state e.operand in
    (match e.unary_operator.token_type with
    | Minus ->
      ( new_state
      , LoxNumber
          (match right with
          | LoxNumber n -> -.n
          | value ->
            raise
            @@ LoxError.TypeError
                 { observed_type = Value.type_of value; expected_type = Number }) )
    | Bang -> new_state, LoxBool (not (is_truthy right))
    | _ -> new_state, LoxNil)
  | Binary e ->
    let new_state, left = evaluate state e.left in
    let new_state, right = evaluate new_state e.right in
    (match e.binary_operator.token_type with
    | Minus -> new_state, binary_arithmetic ( -. ) left right
    | Slash -> new_state, binary_arithmetic ( /. ) left right
    | Star -> new_state, binary_arithmetic ( *. ) left right
    | Plus ->
      (match left, right with
      | LoxNumber _, LoxNumber _ -> new_state, binary_arithmetic ( +. ) left right
      | LoxString l, LoxString r -> new_state, LoxString (l ^ r)
      | LoxString _, v | v, LoxString _ ->
        raise
        @@ LoxError.TypeError { observed_type = Value.type_of v; expected_type = String }
      | v, _ ->
        raise
        @@ LoxError.TypeError { observed_type = Value.type_of v; expected_type = String })
    | Greater -> new_state, binary_comparison Float.( > ) left right
    | GreaterEqual -> new_state, binary_comparison Float.( >= ) left right
    | Less -> new_state, binary_comparison Float.( < ) left right
    | LessEqual -> new_state, binary_comparison Float.( <= ) left right
    | BangEqual -> new_state, LoxBool (not (is_equal left right))
    | EqualEqual -> new_state, LoxBool (is_equal left right)
    | _ -> new_state, LoxNil)
  | Call c ->
    let new_state, callee = evaluate state c.callee in
    let evaluated_args =
      List.map ~f:(fun arg -> evaluate new_state arg) c.arguments
      |> List.map ~f:(fun (_, v) -> v)
    in
    (match callee with
    | Value.LoxFunction f ->
      if List.length evaluated_args = f.arity
      then new_state, Value.call callee evaluated_args
      else
        raise
        @@ LoxError.RuntimeError
             { where = c.paren.line
             ; message =
                 Printf.sprintf
                   "Expected %d arguments but got %d."
                   f.arity
                   (List.length evaluated_args)
             }
    | _ ->
      raise
      @@ LoxError.RuntimeError
           { where = c.paren.line; message = "Can only call functions and classes." })
  | Variable token ->
    (* Environment.print_environment state.state_env; *)
    state, Environment.get_value state.state_env token
  | Assign expr ->
    let new_state, value = evaluate state expr.assign_value in
    new_state, Environment.assign state.state_env expr.name value
  | Logical expr ->
    let new_state, left = evaluate state expr.logical_left in
    (* short-circuit `or` if left evaluates to true *)
    if Scanner.equal_token_type expr.operator.token_type Scanner.Or
    then
      if is_truthy left
      then new_state, left
      else
        evaluate state expr.logical_right
        (* short-circuit `and` if left evaluates to false *)
    else if not (is_truthy left)
    then new_state, left
    else evaluate state expr.logical_right
;;

let rec evaluate_statement (state : state) (statement : Parser.statement) : state =
  match statement with
  | Expression expression ->
    let new_state, _ = evaluate state expression in
    new_state
  | IfStatement s ->
    let new_state, result = evaluate state s.condition in
    if is_truthy result
    then evaluate_statement new_state s.then_branch
    else (
      match s.else_branch with
      | None -> new_state
      | Some branch -> evaluate_statement new_state branch)
  | Print expression ->
    let new_state, result = evaluate state expression in
    let () = result |> Value.string_of |> Stdio.printf "%s\n" in
    new_state
  | ReturnStatement s ->
    let return_value =
      match s.value with
      | None -> Value.LoxNil
      | Some e ->
        let _, v = evaluate state e in
        v
    in
    raise @@ Return return_value
  | FunctionDeclaration f ->
    let env = Environment.init ~enclosing:state.state_env () in
    let func_state = { globals = state.globals; state_env = env } in
    let call_func args =
      (match
         List.iter2
           ~f:(fun (param : Scanner.token) arg ->
             Environment.define func_state.state_env param.lexeme arg)
           f.params
           args
       with
      | Ok _ -> ()
      | Unequal_lengths ->
        raise
        @@ LoxError.RuntimeError
             { where = f.fun_name.line
             ; message = "Unequal number of parameters and arguments applied to function."
             });
      try
        let _ = interpret ~state:func_state f.fun_body in
        Value.LoxNil
      with
      | Return value -> value
    in
    Environment.define
      state.state_env
      f.fun_name.lexeme
      (LoxFunction { arity = List.length f.params; callable = call_func });
    func_state
  | VarDeclaration d ->
    let new_state, value =
      match d.init with
      | Literal l ->
        if Value.equal_eval_type (Value.type_of l.value) (Value.type_of LoxNil)
        then state, Value.LoxNil
        else evaluate state d.init
      | _ -> evaluate state d.init
    in
    Environment.define new_state.state_env d.name.lexeme value;
    new_state
  | WhileStatement statement ->
    let s, v = evaluate state statement.while_condition in
    let new_state, value = ref s, ref v in
    while is_truthy !value do
      new_state := evaluate_statement state statement.body
    done;
    !new_state
  | Block block_statements ->
    let previous_environment = state.state_env in
    let new_environment = Environment.init ~enclosing:state.state_env () in
    (try
       let eval_state =
         interpret ~state:{ state with state_env = new_environment } block_statements
       in
       { eval_state with state_env = previous_environment }
     with
    | _ ->
      (* restore the previous environment even if there was an error *)
      { state with state_env = previous_environment })

and interpret
    ?(state = { globals = Environment.init (); state_env = Environment.init () })
    (statements : Parser.statement list)
    : state
  =
  try
    List.fold_left
      ~f:(fun new_state statement -> evaluate_statement new_state statement)
      ~init:state
      statements
  with
  | LoxError.TypeError error ->
    LoxError.report_runtime_error (LoxError.TypeError error);
    state
  | LoxError.RuntimeError error ->
    LoxError.report_runtime_error (LoxError.RuntimeError error);
    state
;;
