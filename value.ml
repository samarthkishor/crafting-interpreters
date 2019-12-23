type t =
  | LoxBool of bool
  | LoxInt of int
  | LoxNumber of float
  | LoxString of string
  | LoxNil

type eval_type =
  | Bool
  | Number
  | String
  | Nil

let string_of value =
  match value with
  | LoxBool b -> string_of_bool b
  | LoxInt i -> Printf.sprintf "%d" i
  | LoxNumber n -> Printf.sprintf "%g" n
  | LoxString s -> s
  | LoxNil -> "nil"
;;

let string_of_eval_type eval_type =
  match eval_type with
  | Bool -> "Bool"
  | Number -> "Number"
  | String -> "String"
  | Nil -> "Nil"
;;

let type_of value =
  match value with
  | LoxBool _ -> Bool
  | LoxNumber _ | LoxInt _ -> Number
  | LoxString _ -> String
  | LoxNil -> Nil
;;
