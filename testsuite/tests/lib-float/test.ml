(* TEST
*)

let () =
  assert(Float.is_finite 1.);
  assert(Float.is_finite Float.pi);
  assert(not(Float.is_finite Float.infinity));
  assert(not(Float.is_finite Float.nan));
  assert(Float.is_infinite Float.infinity);
  assert(not(Float.is_infinite 1.));
  assert(not(Float.is_infinite Float.nan));
  assert(Float.is_nan Float.nan);
  assert(not(Float.is_nan 1.));
  assert(not(Float.is_nan neg_infinity));
  assert(Float.is_integer 1.);
  assert(Float.is_integer (-1e10));
  assert(not(Float.is_integer 1.5));
  assert(not(Float.is_integer Float.infinity));
  assert(not(Float.is_integer Float.nan));

  assert(Float.trunc 1.5 = 1.);
  assert(Float.trunc (-1.5) = -1.);
  assert(Float.(trunc infinity = infinity));
  assert(Float.(trunc neg_infinity = neg_infinity));
  assert(Float.(is_nan(trunc nan)));

  assert(Float.round 0.5 = 1.);
  assert(Float.round (-0.5) = -1.);
  assert(Float.round 1.5 = 2.);
  assert(Float.round (-1.5) = -2.);
  assert(let x = 0x1.0000000000001p52 in (* x + 0.5 rounds to x +. 1. *)
         Float.round x = x);
  assert(Float.round (Float.next_after 0.5 0.) = 0.);

  assert(Float.(round infinity = infinity));
  assert(Float.(round neg_infinity = neg_infinity));
  assert(Float.(is_nan(round nan)));

  assert(Float.next_after 0x1.FFFFFFFFFFFFFp-2 1. = 0.5);
  assert(Float.next_after 0x1.FFFFFFFFFFFFFp-2 0. = 0x1.FFFFFFFFFFFFEp-2);
  assert(Float.(next_after 0x1.FFFFFFFFFFFFFp-2 infinity = 0.5));
  assert(Float.(next_after 0x1.FFFFFFFFFFFFFp-2 neg_infinity
                = 0x1.FFFFFFFFFFFFEp-2));
  assert(Float.next_after 1. 1. = 1.);
  assert(Float.(is_nan(next_after nan 1.)));
  assert(Float.(is_nan(next_after 3. nan)));

  assert(Float.(succ 0x1.FFFFFFFFFFFFFp-2 = 0.5));
  assert(Float.(pred 0.5 = 0x1.FFFFFFFFFFFFFp-2));
  assert(Float.(succ 0. > 0.));
  assert(Float.(pred 0. < 0.));
  assert(Float.(succ max_float = infinity));
  assert(Float.(pred (-. max_float) = neg_infinity));
  assert(Float.(succ 0. < min_float));
  assert(Float.(succ infinity = infinity));
  assert(Float.(pred neg_infinity = neg_infinity));
  assert(Float.(is_nan(succ nan)));
  assert(Float.(is_nan(pred nan)));

  assert(not(Float.sign_bit 1.));
  assert(Float.sign_bit (-1.));
  assert(not(Float.sign_bit 0.));
  assert(Float.sign_bit (-0.));
  assert(not(Float.sign_bit infinity));
  assert(Float.sign_bit neg_infinity);

  assert(Float.min 1. 2. = 1.);
  assert(Float.min 2. 1. = 1.);
  assert(Float.(is_nan(min 1. nan)));
  assert(Float.(is_nan(min nan 2.)));
  assert(Float.(is_nan(min nan nan)));
  assert(1. /. Float.min (-0.) (+0.) = neg_infinity);
  assert(1. /. Float.min (+0.) (-0.) = neg_infinity);

  assert(Float.max 1. 2. = 2.);
  assert(Float.max 2. 1. = 2.);
  assert(Float.(is_nan(max 1. nan)));
  assert(Float.(is_nan(max nan 2.)));
  assert(Float.(is_nan(max nan nan)));
  assert(1. /. Float.max (-0.) (+0.) = infinity);
  assert(1. /. Float.max (+0.) (-0.) = infinity);

  assert(Float.min_max 1. 2. = (1., 2.));
  assert(Float.min_max 2. 1. = (1., 2.));
  let is_nan2 (x, y) = Float.is_nan x && Float.is_nan y in
  assert(Float.(is_nan2(min_max 1. nan)));
  assert(Float.(is_nan2(min_max nan 2.)));
  assert(Float.(is_nan2(min_max nan nan)));
  assert(let x, y = Float.min_max (-0.) (+0.) in
         1. /. x = neg_infinity && 1. /. y = infinity);
  assert(let x, y = Float.min_max (+0.) (-0.) in
         1. /. x = neg_infinity && 1. /. y = infinity);

  assert(Float.min_num 1. 2. = 1.);
  assert(Float.(min_num 1. nan = 1.));
  assert(Float.(min_num nan 2. = 2.));
  assert(Float.(is_nan(min_num nan nan)));
  assert(1. /. Float.min_num (-0.) (+0.) = neg_infinity);
  assert(1. /. Float.min_num (+0.) (-0.) = neg_infinity);

  assert(Float.max_num 1. 2. = 2.);
  assert(Float.(max_num 1. nan = 1.));
  assert(Float.(max_num nan 2. = 2.));
  assert(Float.(is_nan(max_num nan nan)));
  assert(1. /. Float.max_num (-0.) (+0.) = infinity);
  assert(1. /. Float.max_num (+0.) (-0.) = infinity);

  assert(Float.min_max_num 1. 2. = (1., 2.));
  assert(Float.min_max_num 2. 1. = (1., 2.));
  assert(Float.min_max_num 1. nan = (1., 1.));
  assert(Float.min_max_num nan 1. = (1., 1.));
  assert(Float.(is_nan2(min_max_num nan nan)));
  assert(let x, y = Float.min_max_num (-0.) (+0.) in
         1. /. x = neg_infinity && 1. /. y = infinity);
  assert(let x, y = Float.min_max_num (+0.) (-0.) in
         1. /. x = neg_infinity && 1. /. y = infinity);
;;

let () = print_endline "OK"
