(* TEST
 *)

open Printf
open Effect
open Effect.Deep

type _ t += E : int -> int t

let f () =
  printf "perform effect (E 0)\n%!";
  let v = perform (E 0) in
  printf "perform returns %d\n%!" v;
  v + 1

let h : type a. a t -> ((a, 'b) continuation -> 'b) option = function
  | E v -> Some (fun k ->
      printf "caught effect (E %d). continuing..\n%!" v;
      let v = continue k (v + 1) in
      printf "continue returns %d\n%!" v;
      v + 1)
  | e -> None

let v =
  match_with f ()
  { retc = (fun v -> printf "done %d\n%!" v; v + 1);
    exnc = (fun e -> raise e);
    effc = h }

let () = printf "result=%d\n%!" v
