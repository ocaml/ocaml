(* TEST
 *)

open EffectHandlers
open EffectHandlers.Deep

type _ eff += E : unit eff

let r = ref None
let () =
  match_with (fun _ -> perform E; 42) ()
  { retc = (fun n -> assert (n = 42));
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a eff) ->
      match e with
      | E -> Some (fun (k : (a,_) continuation) ->
          continue k ();
          r := Some (k : (unit, unit) continuation);
          Gc.full_major ();
          print_string "ok\n")
      | _ -> None }
