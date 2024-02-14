(* TEST *)

open Effect
open Effect.Deep

type _ t += Foo : int -> int t

let r =
  match (perform (Foo 3)) with
  | v -> v
  | effect (Foo i), k ->
      begin match continue k (i + 1) with
      | v -> v
      | effect (Foo i), k -> failwith "NO"
      end

let () = Printf.printf "%d\n" r
