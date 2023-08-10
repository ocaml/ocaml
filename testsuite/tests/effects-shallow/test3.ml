(* TEST *)

type e = effect E : unit

exception X

let eff = Effect.create ()

let () =
  Printf.printf "%d\n%!" @@
  match
    Effect.run eff
      (fun () ->
        Printf.printf "in handler. raising X\n%!";
        raise X)
      ()
  with
  | Result v -> v
  | Exn X -> 10
  | Exn e -> raise e
  | Operation(E, _) -> 11
