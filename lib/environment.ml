type t =
  { values : (string, Value.t) Hashtbl.t
  ; mutable enclosing : t option }

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

let rec print_environment ?(level = 0) environment =
  let pad level =
    for _ = 0 to level do
      Printf.printf "\t"
    done
  in
  pad level;
  Printf.printf "Begin environment:\n";
  pad (level + 1);
  Printf.printf "Begin values:\n";
  Hashtbl.iter
    (fun lexeme value ->
      pad (level + 2);
      Printf.printf "%s: %s\n" lexeme (Value.string_of value))
    environment.values;
  pad (level + 1);
  Printf.printf "End values\n";
  let () =
    match environment.enclosing with
    | None ->
      pad (level + 1);
      Printf.printf "No enclosing\n"
    | Some enclosing ->
      pad (level + 1);
      Printf.printf "Begin enclosing:\n";
      print_environment ~level:(level + 1) enclosing;
      pad (level + 1);
      Printf.printf "End enclosing\n"
  in
  pad level;
  Printf.printf "End environment\n"
;;

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
