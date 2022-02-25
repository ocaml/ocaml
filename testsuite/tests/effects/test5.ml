(* TEST
 *)

open Effect
open Effect.Deep

type _ t += Foo : int -> int t

let f () = (perform (Foo 3)) (* 3 + 1 *)
         + (perform (Foo 3)) (* 3 + 1 *)

let r =
  try_with f ()
  { effc = fun (type a) (e : a t) ->
      match e with
      | Foo i -> Some (fun (k : (a, _) continuation) ->
          try_with (continue k) (i + 1)
          { effc = fun (type a) (e : a t) ->
              match e with
              | Foo i -> Some (fun k -> failwith "NO")
              | _ -> None })
      | e -> None }

let () = Printf.printf "%d\n" r
