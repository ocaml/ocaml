external process_fd : int -> string -> unit = "caml_process_fd"

let () =
  for i = 1 to (Array.length Sys.argv) -1
  do
    process_fd i Sys.argv.(i);
  done
