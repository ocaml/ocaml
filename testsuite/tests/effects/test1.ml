(* TEST
 *)

open EffectHandlers
open EffectHandlers.Deep

type _ eff += E : unit eff

let () =
  Printf.printf "%d\n%!" @@
    try_with (fun x -> x) 10
    { effc = (fun (type a) (e : a eff) ->
        match e with
        | E -> Some (fun k -> 11)
        | e -> None) }
