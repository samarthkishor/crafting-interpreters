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

let rec evaluate (expr : Parser.expr) =
  match expr with
  | Literal e -> e.value
  | Grouping e -> evaluate e.expression
  | Unary e ->
    let right = evaluate e.operand in
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
    let left = evaluate e.left in
    let right = evaluate e.right in
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
;;

let interpret expression =
  try evaluate expression |> Value.string_of |> Printf.printf "%s\n"
  with Error.TypeError e -> Error.report_runtime_error (Error.TypeError e)
;;
