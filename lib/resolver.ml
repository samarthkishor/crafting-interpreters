open Base

module Scopes = struct
  type var_status =
    | Declare (** the variable is in the innermost scope and uninitialized *)
    | Define (** the variable is initialized to a value *)

  type t = (string, var_status) Hashtbl.t Stack.t

  let empty (scopes : t) = Stack.length scopes = 0
  let to_list (scopes : t) = scopes |> Stack.to_list |> List.rev

  let pp ppf scopes =
    Caml.Format.open_hovbox 1;
    Caml.Format.print_cut ();
    if empty scopes
    then Caml.Format.fprintf ppf "@[<hov 2>{}@]"
    else
      Stack.iter scopes ~f:(fun scope ->
          if Hashtbl.length scope = 0
          then Caml.Format.fprintf ppf "@[<hov 2>{}@]"
          else (
            Caml.Format.fprintf ppf "@[<hov 1>{@ @]";
            Hashtbl.iteri scope ~f:(fun ~key ~data ->
                Caml.Format.fprintf
                  ppf
                  "@[<hov 2>%s: %s,@ @]"
                  key
                  (match data with
                  | Declare -> "declared"
                  | Define -> "defined"));
            Caml.Format.fprintf ppf "@[<hov 1>}@]"));
    Caml.Format.close_box ()
  ;;
end

module Depths = struct
  type t = (string, int) Hashtbl.t

  let pp ppf values =
    Caml.Format.open_hovbox 1;
    Caml.Format.print_cut ();
    if Hashtbl.length values = 0
    then Caml.Format.fprintf ppf "@[<hov 2>{}@]"
    else (
      Caml.Format.fprintf ppf "@[<hov 1>{@ @]";
      Hashtbl.iteri values ~f:(fun ~key ~data ->
          Caml.Format.fprintf ppf "@[<hov 2>%s: %d,@ @]" key data);
      Caml.Format.fprintf ppf "@[<hov 1>}@]");
    Caml.Format.close_box ()
  ;;
end

type function_type =
  | No_function
  | Some_function
[@@deriving show { with_path = false }]

type t =
  { statements : Parser.statement list
  ; scopes : Scopes.t
  ; depths : Depths.t
  ; current_function : function_type
  ; parsed_statements : Parser.statement list [@opaque]
  }
[@@deriving show { with_path = false }]

let make_resolver statements =
  { statements
  ; scopes = Stack.create ()
  ; depths = Hashtbl.create ~growth_allowed:true ~size:32 (module String)
  ; current_function = No_function
  ; parsed_statements = statements
  }
;;

let begin_scope resolver =
  let () =
    Stack.push
      resolver.scopes
      (Hashtbl.create ~growth_allowed:true ~size:16 (module String))
  in
  resolver
;;

let end_scope resolver =
  let () =
    match Stack.pop resolver.scopes with
    | None | Some _ -> ()
  in
  resolver
;;

(** Update the variable's status as Declared or Defined within the current scope *)
let add_variable name (status : Scopes.var_status) resolver =
  let update_variable_status (name : Scanner.token) status resolver =
    match Stack.pop resolver.scopes with
    | None -> resolver
    | Some scope ->
      let new_scope =
        match Hashtbl.add scope ~key:name.lexeme ~data:status with
        | `Duplicate ->
          Hashtbl.set scope ~key:name.lexeme ~data:status;
          scope
        | `Ok -> scope
      in
      let () = Stack.push resolver.scopes new_scope in
      resolver
  in
  match status with
  | Declare -> update_variable_status name status resolver
  | Define -> update_variable_status name status resolver
;;

(** Resolve the depths of the variables seen so far, keeping track of the scope
    that each variable is in. *)
let resolve_local resolver (var : Scanner.token) =
  let scope_count =
    (* iterate over the scopes in reverse order (bottom-first) *)
    List.fold_until
      (Scopes.to_list resolver.scopes)
      ~init:0
      ~f:(fun count scope ->
        match Hashtbl.find scope var.lexeme with
        | None -> Continue (count + 1)
        | Some _ -> Stop count)
      ~finish:(fun count -> count)
  in
  match Hashtbl.add resolver.depths ~key:var.lexeme ~data:scope_count with
  | `Duplicate ->
    Hashtbl.set resolver.depths ~key:var.lexeme ~data:scope_count;
    resolver
  | `Ok -> resolver
;;

let rec resolve resolver =
  try List.fold resolver.statements ~init:resolver ~f:resolve_statement with
  | LoxError.RuntimeError error ->
    LoxError.report_runtime_error (LoxError.RuntimeError error);
    { resolver with parsed_statements = [] }

and define_function
    (func : Parser.function_declaration)
    (function_type : function_type)
    resolver
  =
  let enclosing_function = resolver.current_function in
  let resolver =
    List.fold func.params ~init:(begin_scope resolver) ~f:(fun acc_resolver param ->
        acc_resolver |> add_variable param Declare |> add_variable param Define)
  in
  let new_resolver =
    resolve { resolver with statements = func.fun_body; current_function = function_type }
  in
  { new_resolver with current_function = enclosing_function } |> end_scope

and resolve_statement resolver statement =
  match statement with
  | Block block_statements ->
    { resolver with statements = block_statements } |> begin_scope |> resolve |> end_scope
  | VarDeclaration v ->
    let new_resolver = add_variable v.name Declare resolver in
    let var_initializer =
      (* None if it's LoxNil, else it's Some expression *)
      (* TODO make literal variable an option type *)
      match v.init with
      | Literal l ->
        if Value.equal_eval_type (Value.type_of l.value) (Value.type_of LoxNil)
        then None
        else Some v.init
      | i -> Some i
    in
    (match var_initializer with
    | None -> add_variable v.name Define new_resolver
    | Some i ->
      resolve { new_resolver with statements = [ Parser.Expression i ] }
      |> add_variable v.name Define)
  | Expression expr ->
    (match expr with
    | Variable var ->
      (match Stack.top resolver.scopes with
      | None -> resolve_local resolver var
      | Some scope ->
        (match Hashtbl.find scope var.lexeme with
        | None -> resolve_local resolver var
        | Some status ->
          (match status with
          | Declare ->
            raise
            @@ LoxError.RuntimeError
                 { where = var.line
                 ; message = "Cannot read local variable in its own initializer."
                 }
          | Define -> resolve_local resolver var)))
    | Assign expr ->
      let new_resolver =
        resolve { resolver with statements = [ Parser.Expression expr.assign_value ] }
      in
      let new_resolver = resolve_local new_resolver expr.name in
      new_resolver
    | Literal _ -> resolver
    | Binary expr ->
      let new_resolver =
        resolve { resolver with statements = [ Parser.Expression expr.left ] }
      in
      resolve { new_resolver with statements = [ Parser.Expression expr.right ] }
    | Call expr ->
      let new_resolver =
        resolve { resolver with statements = [ Parser.Expression expr.callee ] }
      in
      resolve
        { new_resolver with
          statements = List.map expr.arguments ~f:(fun arg -> Parser.Expression arg)
        }
    | Grouping expr ->
      resolve { resolver with statements = [ Parser.Expression expr.expression ] }
    | Logical expr ->
      let new_resolver =
        resolve { resolver with statements = [ Parser.Expression expr.logical_left ] }
      in
      resolve { new_resolver with statements = [ Parser.Expression expr.logical_right ] }
    | Unary expr ->
      resolve { resolver with statements = [ Parser.Expression expr.operand ] })
  | FunctionDeclaration d ->
    resolver
    |> add_variable d.fun_name Declare
    |> add_variable d.fun_name Define
    |> define_function d Some_function
  | IfStatement s ->
    let new_resolver =
      resolve { resolver with statements = [ Parser.Expression s.condition ] }
    in
    let new_resolver = resolve { new_resolver with statements = [ s.then_branch ] } in
    (match s.else_branch with
    | None -> new_resolver
    | Some b -> resolve { new_resolver with statements = [ b ] })
  | Print p -> resolve { resolver with statements = [ Parser.Expression p ] }
  | ReturnStatement r ->
    (match resolver.current_function with
    | No_function ->
      raise
      @@ LoxError.RuntimeError
           { where = r.keyword.line; message = "Cannot return from top-level code." }
    | Some_function ->
      (match r.value with
      | None -> resolver
      | Some e -> resolve { resolver with statements = [ Parser.Expression e ] }))
  | WhileStatement s ->
    let new_resolver =
      resolve { resolver with statements = [ Parser.Expression s.while_condition ] }
    in
    resolve { new_resolver with statements = [ s.body ] }
;;
