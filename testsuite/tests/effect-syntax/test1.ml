(* TEST *)

open Effect
open Effect.Deep

type _ t += E : unit t

let () =
  Printf.printf "%d\n%!" @@
    match 10 with
    | x -> x
    | effect E, k -> 11
