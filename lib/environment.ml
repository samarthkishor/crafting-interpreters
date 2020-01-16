open Base

module Values = struct
  type t = (string, Value.t) Hashtbl.t

  let copy values = Hashtbl.copy values

  let pp ppf values =
    Caml.Format.open_hovbox 1;
    Caml.Format.print_cut ();
    Hashtbl.iteri values ~f:(fun ~key ~data ->
        Caml.Format.fprintf ppf "@[<hov 2>%s: %s@ @]" key (Value.string_of data));
    Caml.Format.close_box ()
  ;;
end

type t =
  { values : Values.t
  ; mutable enclosing : t option
  }
[@@deriving show]

let init ?enclosing () =
  { values = Hashtbl.create ~growth_allowed:true ~size:32 (module String); enclosing }
;;

let rec get_value environment (name : Scanner.token) =
  match Hashtbl.find environment.values name.lexeme with
  | None ->
    (match environment.enclosing with
    | None ->
      raise
      @@ LoxError.RuntimeError
           { where = name.line; message = "Undefined variable '" ^ name.lexeme ^ "'." }
    | Some enclosing -> get_value enclosing name)
  | Some value -> value
;;

let define environment name value =
  match Hashtbl.add environment.values ~key:name ~data:value with
  | `Ok -> ()
  | `Duplicate -> Hashtbl.set environment.values ~key:name ~data:value
;;

let rec assign environment (name : Scanner.token) value =
  match Hashtbl.find environment.values name.lexeme with
  | None ->
    (match environment.enclosing with
    | None ->
      raise
      @@ LoxError.RuntimeError
           { where = name.line; message = "Undefined variable '" ^ name.lexeme ^ "'." }
    | Some enclosing -> assign enclosing name value)
  | Some _ ->
    (match Hashtbl.add environment.values ~key:name.lexeme ~data:value with
    | `Ok -> ()
    | `Duplicate -> Hashtbl.set environment.values ~key:name.lexeme ~data:value);
    value
;;

let copy environment =
  { values = Values.copy environment.values
  ; enclosing =
      (match environment.enclosing with
      | None -> None
      | Some e -> Some { e with values = Values.copy e.values })
  }
;;
