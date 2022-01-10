(* TEST
 *)

open EffectHandlers
open EffectHandlers.Deep

type _ eff += E : unit eff
exception Done

let handle_partial f =
  try_with f ()
  { effc = fun (type a) (e : a eff) ->
      match e with
      | E -> Some (fun k -> assert false)
      | _ -> None }

let f () x = perform E

let () =
  match_with (handle_partial f) ()
  { retc = (fun x -> assert false);
    exnc = (function
      | Done -> print_string "ok\n"
      | e -> raise e);
    effc = fun (type a) (e : a eff) ->
      match e with
      | E -> Some (fun (k : (a, _) continuation) -> discontinue k Done)
      | _ -> None }
