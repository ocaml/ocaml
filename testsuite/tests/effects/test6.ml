(* TEST
 *)

open EffectHandlers
open EffectHandlers.Deep

type _ eff += E : unit eff
            | F : unit eff

let () =
  let ok1 = ref false and ok2 = ref false in
  let f r =
    try perform E with Unhandled -> r := true in
  f ok1;
  Printf.printf "%b\n%!" !ok1;
  try_with f ok2 {
    effc = fun (type a) (e : a eff) ->
      match e with
      | F -> Some (fun k -> assert false)
      | _ -> None
  };
  Printf.printf "%b\n%!" !ok2
