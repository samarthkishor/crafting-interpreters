(* NOTE can't use Scanner here because of cyclic dependencies *)

type parse_error =
  { line : int
  ; lexeme : string
  ; message : string }

type runtime_error =
  { where : int
  ; message : string }

type type_error =
  { observed_type : Value.eval_type
  ; expected_type : Value.eval_type }

type error =
  | RuntimeError of runtime_error
  | TypeError of type_error

exception ParseError of parse_error
exception RuntimeError of runtime_error
exception TypeError of type_error

let had_error = ref false
let had_runtime_error = ref false

let report line where message =
  Printf.eprintf "[line %d] Error %s: %s\n" line where message;
  flush stderr;
  had_error := true
;;

let report_parse_error error =
  let {line; lexeme; message} = error in
  if lexeme = "" (* for Eof *)
  then report line "at end" message
  else report line ("at '" ^ lexeme ^ "'") message
;;

let report_runtime_error (error : error) =
  match error with
  | TypeError e ->
    Printf.eprintf
      "TypeError: observed type %s but expected type %s\n"
      (Value.string_of_eval_type e.observed_type)
      (Value.string_of_eval_type e.expected_type);
    flush stderr;
    had_runtime_error := true
  | RuntimeError e ->
    Printf.eprintf "[line %d] RuntimeError: %s\n" e.where e.message;
    flush stderr;
    had_runtime_error := true
;;

let error line message = report line "" message
