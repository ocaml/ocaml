(* TEST
   modules = "backtrace_c_exn_.c"
   flags = "-g"
   ocamlrunparam += ",b=1"
*)

(* A test for stack backtraces *)
external stubbed_raise : unit -> unit = "caml_498_raise"

let raise_exn () = failwith "exn"
let () = Callback.register "test_raise_exn" raise_exn

let () =
  try
    stubbed_raise ()
  with
  | exn -> Printexc.to_string exn |> print_endline

