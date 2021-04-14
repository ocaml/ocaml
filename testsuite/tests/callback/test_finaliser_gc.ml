(* TEST
*)

let z = ref (0, 1, 2, 3, 4, 5, 6, 7)
let finaliser_pending = ref true

let force_gc_fn _ =
  print_string "Finaliser has run!"; print_newline();
  finaliser_pending := false;
  Gc.full_major ()

let trigger_finaliser () =
  (* Construct finaliser which when run will force
     a major cycle *)
  Gc.finalise force_gc_fn (ref 0);
  (* Allocate a block in the minor heap *)
  let s = String.make 5 'b' in
  (* Spin on the minor heap allocating but keep [s] in a
    register and force a major cycle such that the
    finaliser runs.
    NB: we quit after ~8B words allocated should something
    be broken with finalisers *)
  let x = ref 0 in
  while (!x < 1_000_000_000) && !finaliser_pending do
    z := (!x, !x, !x, !x, !x, !x, !x, !x);
    incr x;
  done;
  s

let _ =
  print_string (trigger_finaliser ()); print_newline();
