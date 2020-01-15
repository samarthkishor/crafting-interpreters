open Base

(* Modules to expose from the Lox library *)
module Value = Value
module Error = Error
module Scanner = Scanner
module Parser = Parser
module Interpreter = Interpreter

let run source =
  Scanner.make_scanner source
  |> Scanner.scan_tokens
  |> Parser.make_parser
  |> Parser.parse
  |> Interpreter.interpret
  |> ignore
;;

let read_file file_name = String.concat (Stdio.In_channel.read_lines file_name) ~sep:"\n"

let run_prompt () =
  while true do
    Stdio.printf "> ";
    Stdio.Out_channel.flush Stdio.stdout;
    let line = Stdio.In_channel.input_line Stdio.stdin in
    (match line with
    | None -> ()
    | Some l -> run l);
    LoxError.had_error := false
  done
;;

let run_file file_name =
  let file_contents = read_file file_name in
  run file_contents;
  if !LoxError.had_error then Caml.exit 65;
  if !LoxError.had_runtime_error then Caml.exit 70
;;
