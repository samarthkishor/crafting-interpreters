open Base

type t =
  | LoxBool of bool
  | LoxInt of int
  | LoxNumber of float
  | LoxString of string
  | LoxNil
  | LoxFunction of lox_function

and lox_function =
  { arity : int
  ; callable : t list -> t
  }
[@@deriving show]

type eval_type =
  | Bool
  | Number
  | String
  | Nil
  | Function
[@@deriving eq]

exception
  TypeError of
    { observed_type : eval_type
    ; expected_type : eval_type
    }

let string_of value =
  match value with
  | LoxBool b -> Bool.to_string b
  | LoxInt i -> Printf.sprintf "%d" i
  | LoxNumber n -> Printf.sprintf "%g" n
  | LoxString s -> s
  | LoxNil -> "nil"
  | LoxFunction _ -> "<function>"
;;

let string_of_eval_type eval_type =
  match eval_type with
  | Bool -> "Bool"
  | Number -> "Number"
  | String -> "String"
  | Nil -> "Nil"
  | Function -> "Function"
;;

let type_of value =
  match value with
  | LoxBool _ -> Bool
  | LoxNumber _ | LoxInt _ -> Number
  | LoxString _ -> String
  | LoxNil -> Nil
  | LoxFunction _ -> Function
;;

let call value args =
  match value with
  | LoxFunction f -> f.callable args
  | value ->
    raise @@ TypeError { observed_type = type_of value; expected_type = Function }
;;
