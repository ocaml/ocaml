(* TEST *)

(* Tests RESUMETERM with extra_args != 0 in bytecode,
   by calling a handler with a tail-continue that returns a function *)

open Effect
open Effect.Deep

type _ eff += E : int eff

let handle comp =
  match comp () with
  | v -> v
  | effect E, k -> continue k 10

let () =
  handle (fun () ->
      Printf.printf "%d\n" (perform E);
      Printf.printf "%d\n") 42
