(* TEST *)

type e = effect E : unit

let e = Effect.create ()

let rec even n =
  if n = 0 then true
  else
    Effect.run_with e odd (n-1)
      { result = Fun.id;
        exn = raise;
        operation =
          (fun (type a) (E : (a, _) operation) ->
            assert false) }
and odd n =
  if n = 0 then false
  else even (n-1)

let _ =
  let n = 100_000 in
  Printf.printf "even %d is %B\n%!" n (even n)
