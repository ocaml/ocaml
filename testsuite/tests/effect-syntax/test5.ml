(* TEST *)

open Effect
open Effect.Deep

type _ eff += Foo : int -> int eff

let f () = (perform (Foo 3)) (* 3 + 1 *)
         + (perform (Foo 3)) (* 3 + 1 *)

let r =
  match f () with
  | v -> v
  | effect (Foo i), k ->
      begin match continue k (i + 1) with
      | v -> v
      | effect (Foo i), k -> failwith "NO"
      end

let () = Printf.printf "%d\n" r
