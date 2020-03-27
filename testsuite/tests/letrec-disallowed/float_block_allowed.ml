(* TEST
   * no-flat-float-array
   ** toplevel
*)

(* these recursive float arrays are allowed when -no-flat-float-array
   is set -- the first array element is not forced on array creation
   anymore *)
let test =
  let rec x = [| y; y |] and y = 1. in
  assert (x = [| 1.; 1. |]);
  assert (y = 1.);
  ()
;;
