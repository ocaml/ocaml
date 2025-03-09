(* TEST

 ocamlopt_flags = "-g";
 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 readonly_files = "waitgroup_stubs.c";
 all_modules = "${readonly_files} waitgroup.ml reperform.ml";
 native;

*)

(* This performs two effects. We trigger race reports in order to check
   correctness of the backtrace in three places:
   - In the outer effect handler after a perform and a reperform;
   - After resuming, back in the deepest computation;
   - After the outermost Effect.match_with has completed. *)
open Printf
open Effect
open Effect.Deep

let print_endline s = Stdlib.print_endline s; flush stdout

type _ t += E1 : int -> int t
type _ t += E2 : int -> int t

let wg1 = Waitgroup.create 2
let wg2 = Waitgroup.create 2
let g_ref1 = ref 0
let g_ref2 = ref 0
let g_ref3 = ref 0

let [@inline never] race =
  function
  | 0 -> g_ref1 := 1
  | 1 -> g_ref2 := 1
  | _ -> g_ref3 := 1

let [@inline never] h () =
  print_endline "Entering h";
  let v = perform (E1 0) in
  race 1;
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
    effc = (fun (type a) (e : a t) ->
        match e with
        | E2 v -> Some (fun (k : (a, _) continuation) ->
              print_endline "E2 handler before continue";
              let v = continue k v in
              print_endline "E2 handler after continue";
              v)
        | e -> None) };
  42

let effh : type a. a t -> ((a, 'b) continuation -> 'b) option = function
  | E1 v -> Some (fun k ->
      print_endline "E1 handler before continue";
      race 0;
      let v = continue k (v + 1) in
      print_endline "E1 handler after continue";
      v + 1
      )
  | e -> None

let [@inline never] fiber1 () =
  ignore @@ match_with fiber2 ()
  { retc = (fun v ->
    print_endline "Value handler"; v + 1);
    exnc = (fun e -> raise e);
    effc = effh };
  41

let[@inline never] main () =
  let v = fiber1 () in
  v + 1

let[@inline never] other_domain () =
  ignore @@ (!g_ref1, !g_ref2, !g_ref3);
  Waitgroup.join wg1;
  Waitgroup.join wg2

let () =
  let d = Domain.spawn other_domain in
  Waitgroup.join wg1;
  let v = main () in
  printf "Result=%d\n%!" v;
  race 2;
  Waitgroup.join wg2;
  Domain.join d
