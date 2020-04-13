(* TEST
 *)

effect E : unit
effect F : unit
let () =
  let ok1 = ref false and ok2 = ref false in
  let f r =
    try perform E with Unhandled -> r := true in
  f ok1;
  Printf.printf "%b\n%!" !ok1;
  (match f ok2 with () -> () | effect F k -> assert false);
  Printf.printf "%b\n%!" !ok2
