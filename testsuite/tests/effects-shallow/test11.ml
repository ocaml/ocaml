(* TEST *)

(* Tests RESUMETERM with extra_args != 0 in bytecode,
   by calling a handler with a tail-continue that returns a function *)

type e = effect E : int

let e = Effect.create ()

let handler comp =
  let rec handle = function
    | Effect.Result x -> x
    | Effect.Exn e -> raise e
    | Effect.Operation(E, k) -> handle (Effect.continue k 10)
  in
  handle (Effect.run e comp ())

let () =
  handler (fun () ->
      Printf.printf "%d\n" (Effect.perform e E);
      Printf.printf "%d\n" 42)
