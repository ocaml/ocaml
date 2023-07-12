(* TEST

 ocamlopt_flags = "-g";
 include unix;
 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
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

let g_ref1 = ref 0
let g_ref2 = ref 0
let g_ref3 = ref 0

let [@inline never] race =
  function
  | 0 -> g_ref1 := 42
  | 1 -> g_ref2 := 42
  | _ -> g_ref3 := 42

let [@inline never] h () =
  print_endline "entering h and perform-ing";
  let v = perform (E 0) in
  print_endline "resuming h";
  race 0;
  print_endline "leaving h";
  v

let [@inline never] g () =
  print_endline "entering g";
  let v = h () in
  print_endline "leaving g";
  v

let [@inline never] f () =
  print_endline "computation, entering f";
  let v = g () in
  print_endline "computation, leaving f";
  v + 1

let effh : type a. a t -> ((a, 'b) continuation -> 'b) option = function
  | E v -> Some (fun k ->
      print_endline "in the effect handler";
      race 1;
      let v = continue k (v + 1) in
      print_endline "handler after continue";
      v + 1
      )
  | e -> None

let[@inline never] main () =
  print_endline "Let's work!";
  ignore (
    match_with f ()
    { retc = (fun v ->
        print_endline "value handler";
        race 2;
        v + 1
      );
      exnc = (fun e -> raise e);
      effc = effh }
  );
  44

let[@inline never] other_domain () =
  ignore (Sys.opaque_identity (!g_ref1, !g_ref2, !g_ref3));
  Unix.sleepf 0.66

let () =
  let d = Domain.spawn other_domain in
  Unix.sleepf 0.33;
  let v = main () in
  printf "result = %d\n" v;
  Domain.join d
