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

  assert(Float.(round infinity = infinity));
  assert(Float.(round neg_infinity = neg_infinity));
  assert(Float.(is_nan(round nan)));

  assert(Float.nextafter 0x1.FFFFFFFFFFFFFp-2 1. = 0.5);
  assert(Float.nextafter 0x1.FFFFFFFFFFFFFp-2 0. = 0x1.FFFFFFFFFFFFEp-2);
  assert(Float.(nextafter 0x1.FFFFFFFFFFFFFp-2 infinity = 0.5));
  assert(Float.(nextafter 0x1.FFFFFFFFFFFFFp-2 neg_infinity
                = 0x1.FFFFFFFFFFFFEp-2));
  assert(Float.nextafter 1. 1. = 1.);
  assert(Float.(is_nan(nextafter nan 1.)));
  assert(Float.(is_nan(nextafter 3. nan)));

  assert(not(Float.signbit 1.));
  assert(Float.signbit (-1.));
  assert(not(Float.signbit 0.));
  assert(Float.signbit (-0.));
  assert(not(Float.signbit infinity));
  assert(Float.signbit neg_infinity);

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

  assert(Float.minmax 1. 2. = (1., 2.));
  assert(Float.minmax 2. 1. = (1., 2.));
  let is_nan2 (x, y) = Float.is_nan x && Float.is_nan y in
  assert(Float.(is_nan2(minmax 1. nan)));
  assert(Float.(is_nan2(minmax nan 2.)));
  assert(Float.(is_nan2(minmax nan nan)));
  assert(let x, y = Float.minmax (-0.) (+0.) in
         1. /. x = neg_infinity && 1. /. y = infinity);
  assert(let x, y = Float.minmax (+0.) (-0.) in
         1. /. x = neg_infinity && 1. /. y = infinity);

  assert(Float.nanmin 1. 2. = 1.);
  assert(Float.(nanmin 1. nan = 1.));
  assert(Float.(nanmin nan 2. = 2.));
  assert(Float.(is_nan(nanmin nan nan)));
  assert(1. /. Float.nanmin (-0.) (+0.) = neg_infinity);
  assert(1. /. Float.nanmin (+0.) (-0.) = neg_infinity);

  assert(Float.nanmax 1. 2. = 2.);
  assert(Float.(nanmax 1. nan = 1.));
  assert(Float.(nanmax nan 2. = 2.));
  assert(Float.(is_nan(nanmax nan nan)));
  assert(1. /. Float.nanmax (-0.) (+0.) = infinity);
  assert(1. /. Float.nanmax (+0.) (-0.) = infinity);
;;

let () = print_endline "OK"
