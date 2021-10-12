(* TEST
*)

(* Tests RESUMETERM with extra_args != 0 in bytecode,
   by calling a handler with a tail-continue that returns a function *)

open EffectHandlers
open EffectHandlers.Deep

type _ eff += E : int eff

let handle comp =
  try_with comp ()
  { effc = fun (type a) (e : a eff) ->
      match e with
      | E -> Some (fun (k : (a,_) continuation) -> continue k 10)
      | _ -> None }

let () =
  handle (fun () ->
      Printf.printf "%d\n" (perform E);
      Printf.printf "%d\n") 42
