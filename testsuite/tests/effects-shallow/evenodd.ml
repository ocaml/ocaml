(* TEST *)

type e = effect E : unit

let e = Effect.create ()

let rec even n =
  if n = 0 then true
  else
    match Effect.run e odd (n-1) with
    | Result x -> x
    | Exn e -> raise e
    | Operation(E, _) -> assert false

and odd n =
  if n = 0 then false
  else even (n-1)

let _ =
  let n = 100_000 in
  Printf.printf "even %d is %B\n%!" n (even n)
