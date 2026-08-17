(* TEST *)

(* IEEE 754 requires sqrt to be correctly rounded, so each expected value is
   the only admissible answer. opaque_identity stops the arguments folding. *)

let opaque = Sys.opaque_identity

let check name got want =
  (* Equality alone treats 0. and -0. as equal, hence the reciprocals. *)
  let ok =
    if Float.is_nan want then Float.is_nan got
    else got = want && 1. /. got = 1. /. want
  in
  if not ok then
    failwith (Printf.sprintf "sqrt %s: got %h, expected %h" name got want)

let () =
  check "0."           (sqrt (opaque 0.))            0.;
  check "-0."          (sqrt (opaque (-0.)))         (-0.);
  check "1."           (sqrt (opaque 1.))            1.;
  check "4."           (sqrt (opaque 4.))            2.;
  check "2."           (sqrt (opaque 2.))            0x1.6a09e667f3bcdp+0;
  check "0.5"          (sqrt (opaque 0.5))           0x1.6a09e667f3bcdp-1;
  check "1e300"        (sqrt (opaque 1e300))         1e150;
  check "max_float"    (sqrt (opaque max_float))     0x1.fffffffffffffp+511;
  check "min_float"    (sqrt (opaque min_float))     0x1p-511;
  check "denormal"     (sqrt (opaque 0x1p-1074))     0x1p-537;
  check "infinity"     (sqrt (opaque infinity))      infinity;
  check "nan"          (sqrt (opaque nan))           nan;
  check "-1."          (sqrt (opaque (-1.)))         nan;
  check "neg_infinity" (sqrt (opaque neg_infinity))  nan;

  for i = 0 to 1000 do
    let x = float_of_int i in
    check (string_of_int i ^ "^2") (sqrt (opaque (x *. x))) x
  done
