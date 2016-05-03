(* a bug in cmmgen.ml provokes a segfault in certain natively compiled
   letrec-bindings involving float arrays *)
let test =
  let rec x = [| y; y |] and y = 1. in
  assert (x = [| 1.; 1. |]);
  assert (y = 1.);
  ()
