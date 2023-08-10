(* TEST *)

open Printf

type e = effect E : int -> int

let eff = Effect.create ()

let f () =
  printf "perform effect (E 0)\n%!";
  let v = Effect.perform eff (E 0) in
  printf "perform returns %d\n%!" v;
  v + 1

let rec handle = function
  | Effect.Result v -> printf "done %d\n%!" v; v + 1
  | Effect.Exn e -> raise e
  | Effect.Operation(E v, k) ->
      printf "caught effect (E %d). continuing..\n%!" v;
      let v = handle (Effect.continue k (v + 1)) in
      printf "continue returns %d\n%!" v;
      v + 1

let v =
  handle (Effect.run eff f ())

let () = printf "result=%d\n%!" v
