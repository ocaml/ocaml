(* TEST *)

open Printf
open Effect

type e = effect E : int -> int

let eff = Effect.create ()

let f () =
  printf "perform effect (E 0)\n%!";
  let v = Effect.perform eff (E 0) in
  printf "perform returns %d\n%!" v;
  v + 1

let h (type k) ((E v) : (k, e) operation) (k : (k, _) continuation) =
  printf "caught effect (E %d). continuing..\n%!" v;
  let v = Effect.continue k (v + 1) in
  printf "continue returns %d\n%!" v;
  v + 1

let v =
  Effect.run_with eff f ()
  { result = (fun v -> printf "done %d\n%!" v; v + 1);
    exn = (fun e -> raise e);
    operation = h }

let () = printf "result=%d\n%!" v
