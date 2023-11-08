(* TEST *)

open Effect
open Effect.Deep

type _ t += E : unit t
exception Done

let handle_partial f =
  try_with f ()
  { effc = fun e ->
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
    effc = fun e ->
      match e with
      | E -> Some (fun k -> discontinue k Done)
      | _ -> None }
