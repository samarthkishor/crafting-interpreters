type t =
  | LoxBool of bool
  | LoxInt of int
  | LoxNumber of float
  | LoxString of string
  | LoxNil

let string_of value =
  match value with
  | LoxBool b -> string_of_bool b
  | LoxInt i -> Printf.sprintf "%d" i
  | LoxNumber n -> Printf.sprintf "%g" n
  | LoxString s -> s
  | LoxNil -> "nil"
;;
