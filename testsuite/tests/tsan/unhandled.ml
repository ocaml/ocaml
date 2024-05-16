(* TEST

 ocamlopt_flags = "-g";
 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 readonly_files = "waitgroup_stubs.c";
 all_modules = "${readonly_files} waitgroup.ml unhandled.ml";
 native;

*)

open Printf
open Effect
open Effect.Deep

let print_endline s = Stdlib.print_endline s; flush stdout

type _ t += E : int -> int t

let wg1 = Waitgroup.create 2
let wg2 = Waitgroup.create 2
let r1 = ref 0
let r2 = ref 0

let [@inline never] race = function
  | 0 -> r1 := 42
  | 1 -> r2 := 42
  | _ -> assert false

let [@inline never] h () =
  print_endline "Entering h";
  let v =
    try perform (E 0)
    with Unhandled _ -> race 1; 1
  in
  print_endline "Leaving h";
  v

let [@inline never] g () =
  print_endline "Entering g";
  let v = h () in
  print_endline "Leaving g";
  v

let f () =
  print_endline "Entering f";
  let v = g () in
  print_endline "Leaving f";
  v + 1

let [@inline never] fiber2 () =
  ignore @@ match_with f ()
  { retc = Fun.id;
    exnc = raise;
    effc = (fun (type a) (e : a t) -> None) };
  42

let effh : type a. a t -> ((a, 'b) continuation -> 'b) option = fun _ -> None

let [@inline never] fiber1 () =
  ignore @@ match_with fiber2 ()
  { retc = (fun v ->
      print_endline "Value handler"; v + 1);
    exnc = (fun e -> raise e);
    effc = effh };
  41

let[@inline never] main () =
  print_endline "Performing an unhandled effect from the main fiber";
  try perform (E 42) with
  | Effect.Unhandled _ -> race 0;
  print_endline "Performing an unhandled effect from another fiber";
  let v = fiber1 () in
  v + 1

let[@inline never] other_domain () =
  ignore @@ (Sys.opaque_identity !r1, !r2);
  Waitgroup.join wg1;
  Waitgroup.join wg2

let () =
  let d = Domain.spawn other_domain in
  Waitgroup.join wg1;
  let v = main () in
  Waitgroup.join wg2;
  printf "Result=%d\n%!" v;
  Domain.join d
