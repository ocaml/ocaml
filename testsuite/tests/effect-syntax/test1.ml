(* TEST *)

open Effect
open Effect.Deep

type _ eff += E : unit eff

let () =
  Printf.printf "%d\n%!" @@
    match 10 with
    | x -> x
    | effect E, k -> 11
