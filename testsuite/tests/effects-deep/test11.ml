(* TEST *)

(* Tests RESUMETERM with extra_args != 0 in bytecode,
   by calling a handler with a tail-continue that returns a function *)

open Effect

type e = effect E : int

let e = Effect.create ()

let handle comp =
  Effect.run_with e comp ()
    { result = Fun.id;
      exn = raise;
      operation =
        (fun (type a) (E : (a, _) operation) (k : (a,_) continuation) ->
          Effect.continue k 10) }

let () =
  handle (fun () ->
      Printf.printf "%d\n" (Effect.perform e E);
      Printf.printf "%d\n") 42
