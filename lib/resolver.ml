open Base

module Scopes = struct
  type var_status =
    | Declare
    | Define

  type t = (string, var_status) Hashtbl.t Stack.t

  let empty scopes = Stack.length scopes = 0

  let pp ppf scopes =
    Caml.Format.open_hovbox 1;
    Caml.Format.print_cut ();
    if empty scopes
    then Caml.Format.fprintf ppf "@[<hov 2>{}@]"
    else
      Stack.iter scopes ~f:(fun scope ->
          if Hashtbl.length scope = 0
          then Caml.Format.fprintf ppf "@[<hov 2>{}@]"
          else
            Hashtbl.iteri scope ~f:(fun ~key ~data ->
                Caml.Format.fprintf
                  ppf
                  "@[<hov 2>%s: %s@ @]"
                  key
                  (match data with
                  | Declare -> "declared"
                  | Define -> "defined")));
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
    else
      Hashtbl.iteri values ~f:(fun ~key ~data ->
          Caml.Format.fprintf ppf "@[<hov 2>%s: %d@ @]" key data);
    Caml.Format.close_box ()
  ;;
end

type t =
  { statements : Parser.statement list
  ; scopes : Scopes.t
  ; depths : Depths.t
  }
[@@deriving show]

let make_resolver statements =
  { statements
  ; scopes = Stack.create ()
  ; depths = Hashtbl.create ~growth_allowed:true ~size:32 (module String)
  }
;;

let add_variable (name : Scanner.token) scopes status : Scopes.t =
  if Scopes.empty scopes
  then scopes
  else (
    let scope =
      match Stack.pop scopes with
      | None -> Hashtbl.create ~growth_allowed:true ~size:16 (module String)
      | Some s -> s
    in
    let new_scope =
      match Hashtbl.add scope ~key:name.lexeme ~data:status with
      | `Duplicate | `Ok -> scope
    in
    Stack.push scopes new_scope;
    scopes)
;;

let resolve_local resolver (var : Scanner.token) =
  let scope_count =
    Stack.fold_until
      resolver.scopes
      ~init:0
      ~f:(fun count scope ->
        match Hashtbl.find scope var.lexeme with
        | None -> Continue (count + 1)
        | Some _ -> Stop count)
      ~finish:(fun count -> count)
  in
  match Hashtbl.add resolver.depths ~key:var.lexeme ~data:scope_count with
  | `Duplicate -> resolver
  | `Ok -> resolver
;;

let rec resolve resolver =
  List.fold resolver.statements ~init:resolver ~f:resolve_statement

and resolve_statement resolver statement =
  match statement with
  | Block block_statements ->
    let () =
      Stack.push
        resolver.scopes
        (Hashtbl.create ~growth_allowed:true ~size:16 (module String))
    in
    let new_resolver = resolve { resolver with statements = block_statements } in
    (match Stack.pop new_resolver.scopes with
    | None | Some _ -> ());
    resolve new_resolver
  | VarDeclaration v ->
    let new_scopes = add_variable v.name resolver.scopes Declare in
    let var_initializer =
      (* TODO make literal variable an option type *)
      match v.init with
      | Literal l ->
        if Value.equal_eval_type (Value.type_of l.value) (Value.type_of LoxNil)
        then None
        else Some v.init
      | i -> Some i
    in
    (match var_initializer with
    | None -> { resolver with scopes = add_variable v.name new_scopes Define }
    | Some i ->
      resolve { resolver with statements = [ Parser.Expression i ]; scopes = new_scopes })
  | Expression expr ->
    (match expr with
    | Variable var ->
      (match Stack.top resolver.scopes with
      | None -> resolver (* TODO figure this out *)
      | Some scope ->
        (match Hashtbl.find scope var.lexeme with
        | None -> resolve_local resolver var
        | Some status ->
          (match status with
          | Declare -> resolve_local resolver var
          | Define ->
            LoxError.error var.line "Cannot read local variable in its own initializer.";
            resolver)))
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
    let new_scopes = add_variable d.fun_name resolver.scopes Declare in
    let new_scopes = add_variable d.fun_name new_scopes Define in
    let () =
      Stack.push new_scopes (Hashtbl.create ~growth_allowed:true ~size:16 (module String))
    in
    let new_scopes =
      List.fold d.params ~init:new_scopes ~f:(fun scopes param ->
          let s = add_variable param scopes Declare in
          add_variable param s Define)
    in
    let new_resolver =
      resolve { resolver with statements = d.fun_body; scopes = new_scopes }
    in
    (match Stack.pop new_resolver.scopes with
    | None | Some _ -> ());
    new_resolver
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
    (match r.value with
    | None -> resolver
    | Some e -> resolve { resolver with statements = [ Parser.Expression e ] })
  | WhileStatement s ->
    let new_resolver =
      resolve { resolver with statements = [ Parser.Expression s.while_condition ] }
    in
    resolve { new_resolver with statements = [ s.body ] }
;;
