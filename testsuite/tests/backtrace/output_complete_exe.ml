(* TEST

flags = "-g -output-complete-exe -ccopt -I -ccopt ${ocamlsrcdir}/runtime"

* bytecode
*)

let () =
  Printexc.record_backtrace true

let f () = if true then raise Exit

let g () =
  print_endline "in g";
  f ();
  print_endline "after f"

let () =
  try
    g ()
  with e ->
    Printexc.print_backtrace stdout
