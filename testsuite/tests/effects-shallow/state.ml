(* TEST *)

type 'a state = effect
  | Get : 'a
  | Set : 'a -> unit

let handle_state (type s) eff init f x =
  let rec handle (state : s) : (_, s state) Effect.step -> _ = function
    | Result res -> res, state
    | Exn e -> raise e
    | Operation(Get, k) ->
        handle state (Effect.continue k state)
    | Operation(Set new_state, k) -> handle new_state (Effect.continue k ())
  in
  handle init (Effect.run eff f x)

let counter : int state Effect.t = Effect.create ()

let comp () =
  Printf.printf "Initial state: %d\n" (Effect.perform counter Get);
  Effect.perform counter (Set 42);
  Printf.printf "Updated state: %d\n" (Effect.perform counter Get);
  Effect.perform counter (Set 43)

let main () =
  let (), i = handle_state counter 0 comp () in
  Printf.printf "Final state: %d\n" i

let _ = main ()
