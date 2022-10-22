(* TEST
*)

(* Tests RESUMETERM with extra_args != 0 in bytecode,
   by calling a handler with a tail-continue that returns a function *)

open Effect
open Effect.Deep

type _ t += E : int t

let handle comp =
  try_with comp ()
  { effc = fun (type a) (e : a t) ->
      match e with
      | E -> Some (fun (k : (a,_) continuation) -> continue k 10)
      | _ -> None }

let () =
  handle (fun () ->
      Printf.printf "%d\n" (perform E);
      Printf.printf "%d\n") 42
