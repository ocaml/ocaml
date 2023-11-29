(* TEST *)

type peek = effect Peek : int
type poke = effect Poke : unit

let peek = Effect.create ()
let poke = Effect.create ()

let rec a i = Effect.perform peek Peek + Random.int i
let rec b i = a i + Random.int i
let rec c i = b i + Random.int i

let rec d i =
  let rec handle = function
    | Effect.Result x -> x
    | Effect.Exn e -> raise e
    | Effect.Operation(Poke, k) -> handle (Effect.continue k ())
  in
  Random.int i + handle (Effect.run poke c i)

let rec e i =
  let rec handle = function
    | Effect.Result x -> x
    | Effect.Exn e -> raise e
    | Effect.Operation(Peek, k) ->
        ignore (Effect.get_callstack k 100);
        handle (Effect.continue k 42)
  in
  Random.int i + handle (Effect.run peek d i)

let _ =
  ignore (e 1);
  print_string "ok\n"
