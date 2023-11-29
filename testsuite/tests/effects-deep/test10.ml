(* TEST *)

open Effect

type peek = effect Peek : int
type poke = effect Poke : unit

let peek = Effect.create ()
let poke = Effect.create ()

let rec a i = Effect.perform peek Peek + Random.int i
let rec b i = a i + Random.int i
let rec c i = b i + Random.int i

let rec d i =
  Random.int i +
  Effect.run_with poke c i
    { result = Fun.id;
      exn = raise;
      operation =
        (fun (type a) (Poke : (a, _) operation) (k : (a,_) continuation) ->
          Effect.continue k ()) }

let rec e i =
  Random.int i +
  Effect.run_with peek d i
    { result = Fun.id;
      exn = raise;
      operation =
        (fun (type a) (Peek : (a, _) operation) (k : (a,_) continuation) ->
          ignore (get_callstack k 100);
          Effect.continue k 42) }

let _ =
  ignore (e 1);
  print_string "ok\n"
