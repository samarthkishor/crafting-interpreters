type runtime_error = {
  where: int;
  message: string
}

exception RuntimeError of runtime_error


let had_error = ref false


let report line where message =
  Printf.eprintf "[line %d] Error %s: %s\n" line where message;
  flush stderr;
  had_error := true


let error line message =
  report line "" message
