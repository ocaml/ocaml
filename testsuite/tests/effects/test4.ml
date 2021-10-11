(* TEST
 *)

open EffectHandlers
open EffectHandlers.Deep

type _ eff += Foo : int -> int eff

let r =
  try_with perform (Foo 3)
  { effc = fun (type a) (e : a eff) ->
      match e with
      | Foo i -> Some (fun (k : (a,_) continuation) ->
          try_with (continue k) (i+1)
          { effc = fun (type a) (e : a eff) ->
              match e with
              | Foo i -> Some (fun k -> failwith "NO")
              | e -> None })
      | e -> None }

let () = Printf.printf "%d\n" r
