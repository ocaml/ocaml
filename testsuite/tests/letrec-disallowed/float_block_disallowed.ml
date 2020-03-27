(* TEST
   * flat-float-array
   ** toplevel
*)

(* when the -flat-float-array optimization is active (standard in
   OCaml versions up to at least 4.07), creating an array inspects its
   first element to decide whether it is a float or not; it would thus
   be unsound to allow to recursively define a float value and an
   array starting with that element (in general we disallow using a
   recursively-defined value in an array literal).
*)
let test =
  let rec x = [| y; y |] and y = 1. in
  assert (x = [| 1.; 1. |]);
  assert (y = 1.);
  ()
;;
