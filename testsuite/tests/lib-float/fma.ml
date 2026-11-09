(* TEST *)

(* fma rounds once, so the first block differs from a separate multiply and
   add. opaque_identity stops the arguments folding. *)

let opaque = Sys.opaque_identity

let check name got want =
  (* Equality alone treats 0. and -0. as equal. *)
  let ok =
    if Float.is_nan want then Float.is_nan got
    else Int64.bits_of_float got = Int64.bits_of_float want
  in
  if not ok then
    failwith (Printf.sprintf "fma %s: got %h, expected %h" name got want)

let fma x y z = Float.fma (opaque x) (opaque y) (opaque z)

let () =
  check "(1+2^-52)(1-2^-53)-1"
    (fma 0x1.0000000000001p+0 0x1.fffffffffffffp-1 (-1.))
    0x1.ffffffffffffep-54;
  check "sqrt2*sqrt2-2"
    (fma 0x1.6a09e667f3bcdp+0 0x1.6a09e667f3bcdp+0 (-2.))
    0x1.3b3efbf5e2229p-52;
  check "(1+2^-27)^2-1"
    (fma 0x1.0000002p+0 0x1.0000002p+0 (-1.))
    0x1.0000001p-26;
  check "one ulp apart"
    (fma (-0x1.4eb5248db40afp-136) (-0x1.5128862c33a4fp+562)
       0x1.5563dab2cd31ep+398)
    0x1.b8d170ee322e6p+426;

  check "3*5-15"       (fma 3. 5. (-15.))            0.;
  check "0*0-0"        (fma 0. 0. (-0.))             0.;
  check "-0*0-0"       (fma (-0.) 0. (-0.))          (-0.);

  check "overflow"     (fma 1e300 1e300 0.)          infinity;
  check "denormal"     (fma 0x1p-537 0x1p-537 0.)    0x1p-1074;
  check "denormal sum" (fma 0x1p-1074 1. 0x1p-1074)  0x1p-1073;
  check "max_float"    (fma max_float 1. 0.)         max_float;

  check "inf*0"        (fma infinity 0. 1.)          nan;
  check "inf*0+nan"    (fma infinity 0. nan)         nan;
  check "inf-inf"      (fma 1. infinity neg_infinity) nan;
  check "nan*1+1"      (fma nan 1. 1.)               nan;

  for i = 0 to 1000 do
    let x = float_of_int i in
    check (string_of_int i ^ "^2+1") (fma x x 1.) (float_of_int (i * i + 1))
  done
