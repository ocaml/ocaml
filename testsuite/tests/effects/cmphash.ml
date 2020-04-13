(* TEST
 *)

effect Yield : unit

let `Cont k =
  match perform Yield with
  | x -> assert false
  | effect Yield k -> `Cont (k : (unit, _) continuation)

let () =
  match k = k with
  | _ -> assert false
  | exception (Invalid_argument _) -> print_endline "ok"

let () =
  match Hashtbl.hash k with
  | _ -> Printf.printf "ok\n"
