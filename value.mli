(* Generic Lox value type *)
type t =
  | LoxBool of bool
  | LoxInt of int
  | LoxNumber of float
  | LoxString of string
  | LoxNil
