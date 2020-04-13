(* TEST
 *)

effect E : unit
exception X

let () =
  Printf.printf "%d\n%!" @@
  try
    Printf.printf "in handler. raising X\n%!";
    raise X
  with
  | X -> 10
  | effect E k -> 11
