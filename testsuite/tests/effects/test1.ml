(* TEST
 *)

effect E : unit
let () =
  Printf.printf "%d\n%!" @@ try 10 with effect E k -> 11
