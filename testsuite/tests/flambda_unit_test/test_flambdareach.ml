open Symbol
open Abstract_identifiers
open Flambda
open Test_utils

let e1 =
  flet x (int 1) (fvar x)

let e2 =
  flets
    [a, int 1;
     b, int 2;
     x, tuple [fvar a;fvar b]]
    (fvar x)

let e3 =
  flet f (ffun_fclos' [x] (fvar x))
    (flet a (int 1)
       (aapply f [a]))

let omega = ffun_fclos' [f] (aapply f [f])
let e4 = flet x omega (aapply x [x])

let e5 =
  flets
    [f, ffun_fclos' [a] (fvar a);
     g, ffun_fclos' [b] (fadd (fvar a) (fvar a));
     x, atuple [f; g]]
    (fapply (afield 1 x) [int 1])

let e6 =
  flet f (ffun_fclos' [a; b] (fadd (fvar a) (fvar a)))
    (fapply (fvar f) [int 1; int 2])

let anf e = Flambdaanf.anf compilation_unit e

let steps code n =
  let anfed = anf code in
  Format.printf "orig %a@.anf %a@."
    Printflambda.flambda code
    Printflambda.flambda anfed;
  Flambdareachability.steps
    ~current_compilation_unit:compilation_unit
    anfed
    n

let main () =
  (* let _r = steps e1 3 in *)
  (* let _r = steps e2 3 in *)
  (* let _r = steps e3 3 in *)
  (* let _r = steps e4 3 in *)
  (* let _r = steps e5 3 in *)
  let _r = steps e6 3 in
  ()

let run () =
  main ();
  Printf.printf "flambdareachability: passed\n%!"
