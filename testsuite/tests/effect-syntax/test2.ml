(* TEST *)

open Printf
open Effect
open Effect.Deep

type _ eff += E : int -> int eff

let f () =
  printf "perform effect (E 0)\n%!";
  let v = perform (E 0) in
  printf "perform returns %d\n%!" v;
  v + 1

let v =
  match f () with
  | v -> printf "done %d\n%!" v; v + 1
  | effect (E v), k ->
      printf "caught effect (E %d). continuing..\n%!" v;
      let v = continue k (v + 1) in
      printf "continue returns %d\n%!" v;
      v + 1

let () = printf "result=%d\n%!" v
