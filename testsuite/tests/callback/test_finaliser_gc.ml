(* TEST
*)

let rec tak (x, y, z as _tuple) =
  if x > y then tak(tak (x-1, y, z), tak (y-1, z, x), tak (z-1, x, y))
           else z

let finaliser_status = ref "Finaliser NOT run"
let z = ref (0, 1, 2, 3, 4, 5, 6, 7)

let force_gc_fn _ =
  finaliser_status := "Finaliser has run!";
  Gc.full_major ()

let trigger_finaliser () =
  (* Construct finaliser which when run will force
     a major cycle *)
  Gc.finalise force_gc_fn (ref 0);
  (* Allocate a block in the minor heap *)
  let s = String.make 5 'b' in
  (* Spin on the minor heap allocating but keep [s] in a
    register and force a major cycle such that the
    finaliser runs *)
  for x = 0 to 10_000_000 do
    z := (x, x, x, x, x, x, x, x);
  done;
  s

let _ =
  print_string (trigger_finaliser ()); print_newline();
  print_string (!finaliser_status); print_newline()
