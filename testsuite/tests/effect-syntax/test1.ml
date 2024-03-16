(* TEST *)

open Effect
open Effect.Deep

effect E : unit

let () =
  Printf.printf "%d\n%!" @@
    match 10 with
    | x -> x
    | effect E, k -> 11
