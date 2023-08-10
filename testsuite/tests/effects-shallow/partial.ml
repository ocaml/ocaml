(* TEST *)

type e = effect E : unit

let eff = Effect.create ()

exception Done

let handle_partial f =
  match Effect.run eff f () with
  | Result x -> x
  | Exn e -> raise e
  | Operation(E, k) -> assert false

let f () x = Effect.perform eff E

let () =
  let rec handle = function
    | Effect.Result x -> assert false
    | Effect.Exn Done -> print_string "ok\n"
    | Effect.Exn e -> raise e
    | Effect.Operation(E, k) -> handle (Effect.discontinue k Done)
  in
  handle (Effect.run eff (handle_partial f) ()) 

