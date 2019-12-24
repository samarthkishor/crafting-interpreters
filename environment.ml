type t =
  { values : (string, Value.t) Hashtbl.t
  ; enclosing : t option }

let init ?enclosing () = {values = Hashtbl.create 32; enclosing}

let rec get_value environment (name : Scanner.token) =
  match Hashtbl.find_opt environment.values name.lexeme with
  | None ->
    (match environment.enclosing with
    | None ->
      raise
      @@ Error.RuntimeError
           {where = name.line; message = "Undefined variable '" ^ name.lexeme ^ "'."}
    | Some enclosing -> get_value enclosing name)
  | Some value -> value
;;

let define environment name value = Hashtbl.add environment.values name value

let rec assign environment (name : Scanner.token) value =
  match Hashtbl.find_opt environment.values name.lexeme with
  | None ->
    (match environment.enclosing with
    | None ->
      raise
      @@ Error.RuntimeError
           {where = name.line; message = "Undefined variable '" ^ name.lexeme ^ "'."}
    | Some enclosing -> assign enclosing name value)
  | Some _ ->
    Hashtbl.replace environment.values name.lexeme value;
    value
;;
