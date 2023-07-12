(* TEST

 ocamlopt_flags = "-g";
 include unix;
 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 native;

*)

open Printf
open Effect
open Effect.Deep

let g_ref = ref 0

let [@inline never] race () =
  g_ref := 42

let [@inline never] g () =
  print_endline "entering g";
  ignore @@ raise Exit;
  print_endline "leaving g";
  12

let [@inline never] f () =
  print_endline "computation, entering f";
  let v = g () in
  print_endline "computation, leaving f";
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

let[@inline never] other_domain () =
  ignore (Sys.opaque_identity !g_ref);
  Unix.sleepf 0.66

let () =
  let d = Domain.spawn other_domain in
  Unix.sleepf 0.33;
  let v = main () in
  printf "result = %d\n" v;
  Domain.join d
