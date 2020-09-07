(* TEST
*)

(* Test that two Random.self_init() in close succession will not result
   in the same PRNG state.
   Note that even when the code is correct this test is expected to fail
   once in 2^30 runs.
*)

let () =
  Random.self_init ();
  let x = Random.bits () in
  Random.self_init ();
  let y = Random.bits () in
  if x = y then print_endline "FAILED" else print_endline "PASSED"
