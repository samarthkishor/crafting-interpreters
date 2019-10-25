open Error


let run source =
  print_endline source


let read_lines name =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let read_file file_name =
  String.concat "\n" (read_lines file_name)


let run_prompt () =
  while true do
    print_string ">";
    flush stdout;
    let line = input_line stdin in
    run line;
    had_error := false;
  done


let run_file file_name =
  let file_contents = read_file file_name in
  run file_contents;
  if !had_error then exit 65


let () =
  let argc = Array.length Sys.argv in
  if argc > 2 then begin
    print_endline "Usage: jlox [script]";
    exit 64
  end
  else if argc = 2 then
    let file_name = Array.get Sys.argv 1 in
    run_file file_name
  else
    run_prompt ()
