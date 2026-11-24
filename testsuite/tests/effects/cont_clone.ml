(* TEST
 modules = "cont_clone_.c";
 {
   bytecode;
 }{
   native;
 }
*)

(* [caml_continuation_clone]: both copies resume, a dropped copy is
   reclaimed, a chain of fibers is copied with its handlers, and a resumed
   continuation cannot be cloned. *)

open Effect
open Effect.Deep

external clone : ('a, 'b) continuation -> ('a, 'b) continuation
  = "cont_clone_stub"

type _ t += E : int t | F : unit t

let stacks () = (Gc.quick_stat ()).Gc.live_stacks_words

(* [E] is performed [depth] frames down, below an exception handler and
   the handler of [F] (another fiber). *)
let gen depth : (int, int) continuation =
  let r : (int, int) continuation option ref = ref None in
  let rec deep n = if n = 0 then perform E else 1 + deep (n - 1) in
  let body () =
    try (try deep depth with Not_found -> -1) with effect F, k -> continue k ()
  in
  ignore (match body () with
    | x -> x
    | effect E, k -> r := Some k; 0);
  Option.get !r

(* Not inlined: flambda would keep the values of a toplevel [let] alive. *)
let[@inline never] drop_copy () =
  let k = gen 2000 in
  ignore (Sys.opaque_identity (clone k));
  ignore (continue k 0)

let () =
  let k = gen 2000 in
  let k' = clone k in
  let a = continue k 1 and b = continue k' 2 in
  let c = discontinue (clone (gen 10)) Not_found in
  Printf.printf "resumed %d %d, discontinued %d\n" a b c;
  Gc.full_major ();
  let before = stacks () in
  drop_copy ();
  Gc.full_major ();
  Gc.full_major ();
  Printf.printf "dropped copy: reclaimed %b\n" (stacks () - before < 1000);
  let k = gen 10 in
  ignore (continue k 0);
  match clone k with
  | _ -> print_endline "clone of a resumed continuation: no exception"
  | exception Continuation_already_resumed ->
    print_endline "clone of a resumed continuation: raised"
