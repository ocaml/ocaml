(* TEST
 *)

open Effect
open Effect.Deep

type _ t += E : unit t

let () =
  try_with perform E
  { effc = fun (type a) (e : a t) ->
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
