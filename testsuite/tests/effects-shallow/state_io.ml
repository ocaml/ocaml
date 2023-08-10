(* TEST *)

type 'a state = effect
  | Get : 'a
  | Set : 'a -> unit

type print = effect
  | Print : string -> unit

let handle_state (type s) eff init f x =
  let rec handle (state : s) : (_, s state) Effect.step -> _ = function
    | Result res -> res, state
    | Exn e -> raise e
    | Operation(Get, k) -> handle state (Effect.continue k state)
    | Operation(Set new_state, k) -> handle new_state (Effect.continue k ())
  in
  handle init (Effect.run eff f x)

let handle_print eff f =
  let rec handle = function
    | Effect.Result x -> x
    | Effect.Exn e -> raise e
    | Effect.Operation(Print s, k) ->
        print_string s;
        handle (Effect.continue k ())
  in
  handle (Effect.run eff f ())

let print = Effect.create ()
let counter = Effect.create ()

let comp () =
  Effect.perform print
    (Print (Printf.sprintf "Initial state: %d\n"
              (Effect.perform counter Get)));
  Effect.perform counter (Set 42);
  Effect.perform print
    (Print (Printf.sprintf "Updated state: %d\n"
              (Effect.perform counter Get)));
  Effect.perform counter (Set 43)

let main () =
  let (), i = handle_print print (handle_state counter 0 comp) in
  Printf.printf "Final state: %d\n" i

let _ = main ()
