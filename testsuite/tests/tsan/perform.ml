(* TEST

 ocamlopt_flags = "-g";
 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 readonly_files = "waitgroup_stubs.c";
 all_modules = "${readonly_files} waitgroup.ml perform.ml";
 native;

*)

(* This performs two effects. We trigger race reports in order to check
   correctness of the backtrace in three places:
    - In the effect handler after performing once;
    - After resuming;
    - In the value handler when the computation returned. *)
open Printf
open Effect
open Effect.Deep

type _ Effect.t += E : int -> int t

let wg1 = Waitgroup.create 2
let wg2 = Waitgroup.create 2
let r1 = ref 0
let r2 = ref 0
let r3 = ref 0

(* Force synchronisation of test output with TSan output to stderr *)
let print_endline s = Stdlib.print_endline s; flush stdout

let [@inline never] race =
  function
  | 0 -> r1 := 42
  | 1 -> r2 := 42
  | _ -> r3 := 42

let [@inline never] h () =
  print_endline "Entering h and perform-ing";
  let v = perform (E 0) in
  print_endline "Resuming h";
  race 0;
  print_endline "Leaving h";
  v

let [@inline never] g () =
  print_endline "Entering g";
  let v = h () in
  print_endline "Leaving g";
  v

let [@inline never] f () =
  print_endline "Computation, entering f";
  let v = g () in
  print_endline "Computation, leaving f";
  v + 1

let effh : type a. a t -> ((a, 'b) continuation -> 'b) option = function
  | E v -> Some (fun k ->
      print_endline "In the effect handler";
      race 1;
      let v = continue k (v + 1) in
      print_endline "Handler after continue";
      v + 1
      )
  | e -> None

let[@inline never] main () =
  print_endline "Let's work!";
  ignore (
    match_with f ()
    { retc = (fun v ->
        print_endline "Value handler";
        race 2;
        v + 1
      );
      exnc = (fun e -> raise e);
      effc = effh }
  );
  42

let[@inline never] other_domain () =
  ignore (Sys.opaque_identity (!r1, !r2, !r3));
  Waitgroup.join wg1;
  Waitgroup.join wg2

let () =
  let d = Domain.spawn other_domain in
  Waitgroup.join wg1;
  let v = main () in
  Waitgroup.join wg2;
  eprintf "Result = %d\n" v;
  Domain.join d
