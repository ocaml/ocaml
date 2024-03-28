(* TEST *)

open Effect
open Effect.Deep

effect E : unit
exception X

let () =
  Printf.printf "%d\n%!" @@
  match (Printf.printf "in handler. raising X\n%!"; raise X) with
  | v -> v
  | exception X -> 10
  | effect E, _ -> 11
