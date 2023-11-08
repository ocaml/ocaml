(* TEST *)

open Effect
open Effect.Deep

type _ t += Foo : int -> int t

let r =
  try_with perform (Foo 3)
  { effc = fun e ->
      match e with
      | Foo i -> Some (fun k ->
          try_with (continue k) (i+1)
          { effc = fun e ->
              match e with
              | Foo i -> Some (fun k -> failwith "NO")
              | e -> None })
      | e -> None }

let () = Printf.printf "%d\n" r
