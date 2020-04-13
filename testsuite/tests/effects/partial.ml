(* TEST
 *)

effect E : unit
exception Done

let handle_partial f =
  try f () with
  | effect E k -> assert false

let f () x = perform E

let () =
  match
    let g = handle_partial f in
    g ()
  with
  | x -> assert false
  | exception Done -> print_string "ok\n"
  | effect E k -> discontinue k Done
