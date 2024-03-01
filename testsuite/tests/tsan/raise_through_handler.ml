(* TEST

 ocamlopt_flags = "-g";
 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 readonly_files = "waitgroup_stubs.c";
 all_modules = "${readonly_files} waitgroup.ml raise_through_handler.ml";
 native;

*)

open Printf
open Effect
open Effect.Deep

let wg = Waitgroup.create 2
let r = ref 0

let [@inline never] race () =
  r := 42;
  Waitgroup.join wg

let [@inline never] g () =
  print_endline "Entering g";
  ignore @@ raise Exit;
  print_endline "Leaving g";
  12

let [@inline never] f () =
  print_endline "Computation, entering f";
  let v = g () in
  print_endline "Computation, leaving f";
  v + 1

let effh : type a. a t -> ((a, 'b) continuation -> 'b) option = fun _ -> None

let[@inline never] main () =
  print_endline "Let's work!";
  (try
    ignore (
      match_with f ()
      { retc = (fun v -> v + 1);
        exnc = (fun e -> raise e);
        effc = effh }
    )
  with Exit ->
    print_endline "In exception handler";
    race ();
  );
  44

let [@inline never] reader () =
  Waitgroup.join wg;
  ignore (Sys.opaque_identity !r)

let () =
  let d = Domain.spawn reader in
  let v = main () in
  printf "Result = %d\n" v;
  Domain.join d
