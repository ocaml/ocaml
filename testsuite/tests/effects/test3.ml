(* TEST
 *)

open EffectHandlers
open EffectHandlers.Deep

type _ eff += E : unit eff
exception X

let () =
  Printf.printf "%d\n%!" @@
  match_with (fun () ->
    Printf.printf "in handler. raising X\n%!";
    raise X) ()
    { retc = (fun v -> v);
      exnc = (function
        | X -> 10
        | e -> raise e);
      effc = (fun (type a) (e : a eff) ->
        match e with
        | E -> Some (fun k -> 11)
        | e -> None) }
