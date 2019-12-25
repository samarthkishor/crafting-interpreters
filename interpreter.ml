type t = {environment : Environment.t}

let is_truthy value =
  match value with Value.LoxNil -> false | Value.LoxBool b -> b | _ -> true
;;

let is_equal left right =
  let open Value in
  match left, right with LoxNil, LoxNil -> true | LoxNil, _ -> false | l, r -> l = r
;;

let binary_arithmetic operator left right =
  let open Value in
  match left, right with
  | LoxNumber l, LoxNumber r -> LoxNumber (operator l r)
  | LoxNumber _, v ->
    raise @@ Error.TypeError {observed_type = type_of v; expected_type = Number}
  | v, LoxNumber _ ->
    raise @@ Error.TypeError {observed_type = type_of v; expected_type = Number}
  | v, _ -> raise @@ Error.TypeError {observed_type = type_of v; expected_type = Number}
;;

let binary_comparison operator left right =
  let open Value in
  match left, right with
  | LoxNumber l, LoxNumber r -> LoxBool (operator l r)
  | LoxNumber _, v ->
    raise @@ Error.TypeError {observed_type = type_of v; expected_type = Number}
  | v, LoxNumber _ ->
    raise @@ Error.TypeError {observed_type = type_of v; expected_type = Number}
  | v, _ -> raise @@ Error.TypeError {observed_type = type_of v; expected_type = Number}
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
          @@ Error.TypeError {observed_type = Value.type_of value; expected_type = Number})
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
        raise @@ Error.TypeError {observed_type = Value.type_of v; expected_type = String}
      | v, _ ->
        raise @@ Error.TypeError {observed_type = Value.type_of v; expected_type = String})
    | Greater -> binary_comparison ( > ) left right
    | GreaterEqual -> binary_comparison ( >= ) left right
    | Less -> binary_comparison ( < ) left right
    | LessEqual -> binary_comparison ( <= ) left right
    | BangEqual -> LoxBool (not (is_equal left right))
    | EqualEqual -> LoxBool (is_equal left right)
    | _ -> LoxNil)
  | Variable token -> Environment.get_value environment token
  | Assign expr ->
    let value = evaluate environment expr.assign_value in
    Environment.assign environment expr.name value
;;

let rec eval_statement environment statement =
  match statement with
  | Parser.Expression expression -> ignore (evaluate environment expression)
  | Parser.Print expression ->
    evaluate environment expression |> Value.string_of |> Printf.printf "%s\n"
  | VarDeclaration d ->
    let value =
      match d.init with
      | Literal l ->
        if l.value = LoxNil then Value.LoxNil else evaluate environment d.init
      | _ -> evaluate environment d.init
    in
    Environment.define environment d.name.lexeme value
  | Block block_statements ->
    let previous_environment = environment in
    let new_environment = Environment.init ~enclosing:previous_environment () in
    (try
       interpret ~environment:new_environment block_statements
       (* restore the previous environment even if there was an error *)
     with error ->
       environment.enclosing <- Some previous_environment;
       raise error);
    environment.enclosing <- Some previous_environment

and interpret ?(environment = Environment.init ()) (statements : Parser.statement list) =
  try List.iter (fun statement -> eval_statement environment statement) statements with
  | Error.TypeError error -> Error.report_runtime_error (Error.TypeError error)
  | Error.RuntimeError error -> Error.report_runtime_error (Error.RuntimeError error)
;;
