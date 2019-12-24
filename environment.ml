type t = {values : (string, Value.t) Hashtbl.t}

let init () = {values = Hashtbl.create 32}

let get_value environment (name : Scanner.token) =
  match Hashtbl.find_opt environment.values name.lexeme with
  | None ->
    raise
    @@ Error.RuntimeError
         {where = name.line; message = "Undefined variable '" ^ name.lexeme ^ "'."}
  | Some value -> value
;;

let define environment name value = Hashtbl.add environment.values name value
