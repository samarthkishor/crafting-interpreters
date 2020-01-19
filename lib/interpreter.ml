open Base

type t =
  { mutable state_env : Environment.t
  ; mutable globals : Environment.t
  ; locals : Resolver.Depths.t
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

let look_up_variable state (token : Scanner.token) =
  match Hashtbl.find state.locals token.lexeme with
  | None -> Environment.get_value state.state_env token
  | Some distance -> Environment.get_at_distance state.state_env distance token
;;

let rec evaluate state (expr : Parser.expr) =
  match expr with
  | Literal e -> e.value
  | Grouping e -> evaluate state e.expression
  | Unary e ->
    let right = evaluate state e.operand in
    (match e.unary_operator.token_type with
    | Minus ->
      LoxNumber
        (match right with
        | LoxNumber n -> -.n
        | value ->
          raise
          @@ LoxError.TypeError
               { observed_type = Value.type_of value; expected_type = Number })
    | Bang -> LoxBool (not (is_truthy right))
    | _ -> LoxNil)
  | Binary e ->
    let left = evaluate state e.left in
    let right = evaluate state e.right in
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
        @@ LoxError.TypeError { observed_type = Value.type_of v; expected_type = String }
      | v, _ ->
        raise
        @@ LoxError.TypeError { observed_type = Value.type_of v; expected_type = String })
    | Greater -> binary_comparison Float.( > ) left right
    | GreaterEqual -> binary_comparison Float.( >= ) left right
    | Less -> binary_comparison Float.( < ) left right
    | LessEqual -> binary_comparison Float.( <= ) left right
    | BangEqual -> LoxBool (not (is_equal left right))
    | EqualEqual -> LoxBool (is_equal left right)
    | _ -> LoxNil)
  | Call c ->
    let callee = evaluate state c.callee in
    let evaluated_args = List.map ~f:(fun arg -> evaluate state arg) c.arguments in
    (match callee with
    | Value.LoxFunction f ->
      if List.length evaluated_args = f.arity
      then Value.call callee evaluated_args
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
  | Variable token -> look_up_variable state token
  | Assign expr ->
    let value = evaluate state expr.assign_value in
    (match Hashtbl.find state.locals (Parser.string_of_expr expr.assign_value) with
    | None -> Environment.assign state.state_env expr.name value
    | Some distance ->
      Environment.assign_at_distance state.state_env expr.name value distance)
  | Logical expr ->
    let left = evaluate state expr.logical_left in
    (* short-circuit `or` if left evaluates to true *)
    if Scanner.equal_token_type expr.operator.token_type Scanner.Or
    then
      if is_truthy left
      then left
      else
        evaluate state expr.logical_right
        (* short-circuit `and` if left evaluates to false *)
    else if not (is_truthy left)
    then left
    else evaluate state expr.logical_right
;;

let rec evaluate_statement (state : t) statement =
  match statement with
  | Parser.Expression expression -> ignore (evaluate state expression)
  | Parser.IfStatement s ->
    if is_truthy (evaluate state s.condition)
    then evaluate_statement state s.then_branch
    else (
      match s.else_branch with
      | None -> ()
      | Some branch -> evaluate_statement state branch)
  | Parser.Print expression ->
    evaluate state expression |> Value.string_of |> Stdio.printf "%s\n"
  | ReturnStatement s ->
    let return_value =
      match s.value with
      | None -> Value.LoxNil
      | Some e -> evaluate state e
    in
    raise @@ Return return_value
  | FunctionDeclaration f ->
    let env = Environment.init ~enclosing:state.state_env () in
    let func_state : Environment.t =
      { values = state.state_env.values; enclosing = Some env }
    in
    let call_func args =
      let new_env =
        match func_state.enclosing with
        | None -> Environment.init ()
        | Some enclosing -> Environment.init ~enclosing ()
      in
      (match
         List.iter2
           ~f:(fun (param : Scanner.token) arg ->
             Environment.define new_env param.lexeme arg)
           f.params
           args
       with
      | Ok _ -> ()
      | Unequal_lengths ->
        raise
        @@ LoxError.RuntimeError
             { where = f.fun_name.line
             ; message = "Unequal number of arguments and parameters to function."
             });
      try
        interpret { state with state_env = new_env } f.fun_body;
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
        if Value.equal_eval_type (Value.type_of l.value) (Value.type_of LoxNil)
        then Value.LoxNil
        else evaluate state d.init
      | _ -> evaluate state d.init
    in
    Environment.define state.state_env d.name.lexeme value
  | WhileStatement s ->
    while is_truthy (evaluate state s.while_condition) do
      evaluate_statement state s.body
    done
  | Block block_statements ->
    (* need to copy the Environment because it's passed by reference *)
    let previous_environment = Environment.copy state.state_env in
    let new_environment = Environment.init ~enclosing:state.state_env () in
    (try interpret { state with state_env = new_environment } block_statements with
    | error ->
      (* restore the previous environment even if there was an error *)
      state.state_env <- previous_environment;
      raise error);
    state.state_env <- previous_environment

and interpret state (statements : Parser.statement list) =
  try List.iter ~f:(fun statement -> evaluate_statement state statement) statements with
  | LoxError.TypeError error -> LoxError.report_runtime_error (LoxError.TypeError error)
  | LoxError.RuntimeError error ->
    LoxError.report_runtime_error (LoxError.RuntimeError error)
;;

let make_interpreter (resolver : Resolver.t) =
  let state =
    { state_env = Environment.init ()
    ; globals = Environment.init ()
    ; locals = resolver.depths
    }
  in
  interpret state resolver.parsed_statements
;;
