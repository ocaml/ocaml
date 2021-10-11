(* TEST
 *)

open EffectHandlers
open EffectHandlers.Deep

type _ eff += E : unit eff

let () =
  try_with perform E
  { effc = fun (type a) (e : a eff) ->
      match e with
      | E -> Some (fun k ->
          begin match k = k with
          | _ -> assert false
          | exception (Invalid_argument _) -> print_endline "ok"
          end;
          begin match Hashtbl.hash k with
          | _ -> print_endline "ok"
          end)
      | e -> None }
