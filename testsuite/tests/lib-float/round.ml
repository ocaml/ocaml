(* TEST *)

(* Rounding to a float integer is exact, so each expected value is the only
   admissible answer. [Float.round] takes halfway cases away from zero, so 2.5
   is 3. and not the even 2. opaque_identity stops the arguments folding. *)

let opaque = Sys.opaque_identity

let check name x got want =
  (* Equality alone treats 0. and -0. as equal. *)
  let ok =
    if Float.is_nan want then Float.is_nan got
    else Int64.bits_of_float got = Int64.bits_of_float want
  in
  if not ok then
    failwith (Printf.sprintf "%s %h: got %h, expected %h" name x got want)

let check_val name x got want =
  if not (got = want) then
    failwith (Printf.sprintf "%s %h: got %h, expected %h" name x got want)

let case x ~floor:f ~ceil:c ~trunc:t ~round:r =
  check "floor" x (Float.floor (opaque x)) f;
  check "ceil"  x (Float.ceil  (opaque x)) c;
  check "trunc" x (Float.trunc (opaque x)) t;
  check "round" x (Float.round (opaque x)) r;
  check "Stdlib.floor" x (floor (opaque x)) f;
  check "Stdlib.ceil"  x (ceil  (opaque x)) c

let () =
  case 0.    ~floor:0.    ~ceil:0.    ~trunc:0.    ~round:0.;
  case (-0.) ~floor:(-0.) ~ceil:(-0.) ~trunc:(-0.) ~round:(-0.);
  case 1.    ~floor:1.    ~ceil:1.    ~trunc:1.    ~round:1.;
  case (-1.) ~floor:(-1.) ~ceil:(-1.) ~trunc:(-1.) ~round:(-1.);

  case 0.5    ~floor:0.    ~ceil:1.    ~trunc:0.    ~round:1.;
  case (-0.5) ~floor:(-1.) ~ceil:(-0.) ~trunc:(-0.) ~round:(-1.);
  case 1.5    ~floor:1.    ~ceil:2.    ~trunc:1.    ~round:2.;
  case (-1.5) ~floor:(-2.) ~ceil:(-1.) ~trunc:(-1.) ~round:(-2.);
  case 2.5    ~floor:2.    ~ceil:3.    ~trunc:2.    ~round:3.;
  case (-2.5) ~floor:(-3.) ~ceil:(-2.) ~trunc:(-2.) ~round:(-3.);

  case 0x1.fffffffffffffp-2
    ~floor:0. ~ceil:1. ~trunc:0. ~round:0.;
  case (-0x1.fffffffffffffp-2)
    ~floor:(-1.) ~ceil:(-0.) ~trunc:(-0.) ~round:(-0.);

  case 3.7    ~floor:3.    ~ceil:4.    ~trunc:3.    ~round:4.;
  case (-3.7) ~floor:(-4.) ~ceil:(-3.) ~trunc:(-3.) ~round:(-4.);

  case 0x1p-1074    ~floor:0.    ~ceil:1.    ~trunc:0.    ~round:0.;
  case (-0x1p-1074) ~floor:(-1.) ~ceil:(-0.) ~trunc:(-0.) ~round:(-0.);

  case 0x1.fffffffffffffp+51
    ~floor:4503599627370495. ~ceil:4503599627370496.
    ~trunc:4503599627370495. ~round:4503599627370496.;
  case (-0x1.fffffffffffffp+51)
    ~floor:(-4503599627370496.) ~ceil:(-4503599627370495.)
    ~trunc:(-4503599627370495.) ~round:(-4503599627370496.);

  case 1e300    ~floor:1e300    ~ceil:1e300    ~trunc:1e300    ~round:1e300;
  case (-1e300) ~floor:(-1e300) ~ceil:(-1e300) ~trunc:(-1e300) ~round:(-1e300);
  case max_float
    ~floor:max_float ~ceil:max_float ~trunc:max_float ~round:max_float;

  case infinity
    ~floor:infinity ~ceil:infinity ~trunc:infinity ~round:infinity;
  case neg_infinity
    ~floor:neg_infinity ~ceil:neg_infinity
    ~trunc:neg_infinity ~round:neg_infinity;
  case nan ~floor:nan ~ceil:nan ~trunc:nan ~round:nan;

  for i = -1000 to 1000 do
    let x = float_of_int i /. 8. in
    let fdiv a b = if a >= 0 then a / b else - ((- a + b - 1) / b) in
    let want name v = check_val name x v in
    want "floor" (Float.floor (opaque x)) (float_of_int (fdiv i 8));
    want "ceil"  (Float.ceil  (opaque x)) (float_of_int (- (fdiv (- i) 8)));
    want "trunc" (Float.trunc (opaque x)) (float_of_int (i / 8));
    want "round" (Float.round (opaque x))
      (float_of_int (if i >= 0 then (i + 4) / 8 else - ((- i + 4) / 8)))
  done
