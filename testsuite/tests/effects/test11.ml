(* TEST
*)

(* Tests RESUMETERM with extra_args != 0 in bytecode,
   by calling a handler with a tail-continue that returns a function *)
effect E : int

let handle comp =
  match comp () with
  | f -> f
  | effect E k ->
     continue k 10

let () =
  handle (fun () ->
      Printf.printf "%d\n" (perform E);
      Printf.printf "%d\n") 42
