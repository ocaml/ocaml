(* TEST *)

open Effect
open Effect.Deep

type _ t += E : unit t

let r = ref None
let () =
  match_with (fun _ -> perform E; 42) ()
  { retc = (fun n -> assert (n = 42));
    exnc = (fun e -> raise e);
    effc = fun e ->
      match e with
      | E -> Some (fun k ->
          continue k ();
          r := Some (k : (unit, unit) continuation);
          Gc.full_major ();
          print_string "ok\n")
      | _ -> None }
