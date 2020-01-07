let () =
  let argc = Array.length Sys.argv in
  if argc > 2
  then (
    print_endline "Usage: jlox [script]";
    exit 64)
  else if argc = 2
  then (
    let file_name = Sys.argv.(1) in
    Lox.run_file file_name)
  else Lox.run_prompt ()
;;
