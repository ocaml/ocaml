(* TEST
*)

let is_nan2 (x, y) = Float.is_nan x && Float.is_nan y

type test = True of (unit -> bool)
          | False of (unit -> bool)
          | Equal of ((unit -> float) * float)
          | Pair of ((unit -> float * float) * (float * float))

let cases = [
  ( 1, True (fun () -> Float.is_finite 1.));
  ( 2, True (fun () -> Float.is_finite Float.pi));
  ( 3, False(fun () -> Float.is_finite Float.infinity));
  ( 4, False(fun () -> Float.is_finite Float.nan));
  ( 5, True (fun () -> Float.is_infinite Float.infinity));
  ( 6, False(fun () -> Float.is_infinite 1.));
  ( 7, False(fun () -> Float.is_infinite Float.nan));
  ( 8, True (fun () -> Float.is_nan Float.nan));
  ( 9, False(fun () -> Float.is_nan 1.));
  (10, False(fun () -> Float.is_nan neg_infinity));
  (11, True (fun () -> Float.is_integer 1.));
  (12, True (fun () -> Float.is_integer (-1e10)));
  (13, False(fun () -> Float.is_integer 1.5));
  (14, False(fun () -> Float.is_integer Float.infinity));
  (15, False(fun () -> Float.is_integer Float.nan));

  (16, Equal((fun () -> Float.trunc 1.5), 1.));
  (17, Equal((fun () -> Float.trunc (-1.5)), -1.));
  (18, Equal(Float.((fun () -> trunc infinity), infinity)));
  (19, Equal(Float.(((fun () -> trunc neg_infinity), neg_infinity))));
  (20, True (fun () -> Float.(is_nan(trunc nan))));

  (21, Equal((fun () -> Float.round 0.5), 1.));
  (22, Equal((fun () -> Float.round (-0.5)), -1.));
  (23, Equal((fun () -> Float.round 1.5), 2.));
  (24, Equal((fun () -> Float.round (-1.5)), -2.));
  (25, let x = 0x1.0000000000001p52 in (* x + 0.5 rounds to x +. 1. *)
       Equal((fun () -> Float.round x), x));
  (26, Equal((fun () -> Float.round (Float.next_after 0.5 0.)), 0.));

  (27, Equal(Float.((fun () -> round infinity), infinity)));
  (28, Equal(Float.((fun () -> round neg_infinity), neg_infinity)));
  (29, True (fun () -> Float.(is_nan(round nan))));

  (30, Equal((fun () -> Float.next_after 0x1.FFFFFFFFFFFFFp-2 1.), 0.5));
  (31, Equal((fun () -> Float.next_after 0x1.FFFFFFFFFFFFFp-2 0.), 0x1.FFFFFFFFFFFFEp-2));
  (32, Equal(Float.((fun () -> next_after 0x1.FFFFFFFFFFFFFp-2 infinity), 0.5)));
  (33, Equal(Float.((fun () -> next_after 0x1.FFFFFFFFFFFFFp-2 neg_infinity), 0x1.FFFFFFFFFFFFEp-2)));
  (34, Equal((fun () -> Float.next_after 1. 1.), 1.));
  (35, True (fun () -> Float.(is_nan(next_after nan 1.))));
  (36, True (fun () -> Float.(is_nan(next_after 3. nan))));

  (37, Equal(Float.((fun () -> succ 0x1.FFFFFFFFFFFFFp-2), 0.5)));
  (38, Equal(Float.((fun () -> pred 0.5), 0x1.FFFFFFFFFFFFFp-2)));
  (39, True (Float.(fun () -> succ 0. > 0.)));
  (40, True (Float.(fun () -> pred 0. < 0.)));
  (41, Equal(Float.((fun () -> succ max_float), infinity)));
  (42, Equal(Float.((fun () -> pred (-. max_float)), neg_infinity)));
  (43, True (Float.(fun () -> succ 0. < min_float)));
  (44, Equal(Float.((fun () -> succ infinity), infinity)));
  (45, Equal(Float.((fun () -> pred neg_infinity), neg_infinity)));
  (46, True (Float.(fun () -> is_nan(succ nan))));
  (47, True (Float.(fun () -> is_nan(pred nan))));

  (48, False(fun () -> Float.sign_bit 1.));
  (49, True (fun () -> Float.sign_bit (-1.)));
  (50, False(fun () -> Float.sign_bit 0.));
  (51, True (fun () -> Float.sign_bit (-0.)));
  (52, False(fun () -> Float.sign_bit infinity));
  (53, True (fun () -> Float.sign_bit neg_infinity));

  (54, Equal((fun () -> Float.min 1. 2.), 1.));
  (55, Equal((fun () -> Float.min 2. 1.), 1.));
  (56, True (fun () -> Float.(is_nan(min 1. nan))));
  (57, True (fun () -> Float.(is_nan(min nan 2.))));
  (58, True (fun () -> Float.(is_nan(min nan nan))));
  (59, Equal((fun () -> 1. /. Float.min (-0.) (+0.)), neg_infinity));
  (60, Equal((fun () -> 1. /. Float.min (+0.) (-0.)), neg_infinity));

  (61, Equal((fun () -> Float.max 1. 2.), 2.));
  (62, Equal((fun () -> Float.max 2. 1.), 2.));
  (63, True (fun () -> Float.(is_nan(max 1. nan))));
  (64, True (fun () -> Float.(is_nan(max nan 2.))));
  (65, True (fun () -> Float.(is_nan(max nan nan))));
  (66, Equal((fun () -> 1. /. Float.max (-0.) (+0.)), infinity));
  (67, Equal((fun () -> 1. /. Float.max (+0.) (-0.)), infinity));

  (68, Pair ((fun () -> Float.min_max 1. 2.), (1., 2.)));
  (69, Pair ((fun () -> Float.min_max 2. 1.), (1., 2.)));
  (70, True (fun () -> Float.(is_nan2(min_max 1. nan))));
  (71, True (fun () -> Float.(is_nan2(min_max nan 2.))));
  (72, True (fun () -> Float.(is_nan2(min_max nan nan))));
  (73, Pair ((fun () -> let x, y = Float.min_max (-0.) (+0.) in
                        (1. /. x, 1. /. y)), (neg_infinity, infinity)));
  (74, Pair ((fun () -> let x, y = Float.min_max (+0.) (-0.) in
                        (1. /. x, 1. /. y)), (neg_infinity, infinity)));

  (75, Equal((fun () -> Float.min_num 1. 2.), 1.));
  (76, Equal(Float.((fun () -> min_num 1. nan), 1.)));
  (77, Equal(Float.((fun () -> min_num nan 2.), 2.)));
  (78, True (fun () -> Float.(is_nan(min_num nan nan))));
  (79, Equal((fun () -> 1. /. Float.min_num (-0.) (+0.)), neg_infinity));
  (80, Equal((fun () -> 1. /. Float.min_num (+0.) (-0.)), neg_infinity));

  (81, Equal((fun () -> Float.max_num 1. 2.), 2.));
  (82, Equal(Float.((fun () -> max_num 1. nan), 1.)));
  (83, Equal(Float.((fun () -> max_num nan 2.), 2.)));
  (84, True (fun () -> Float.(is_nan(max_num nan nan))));
  (85, Equal((fun () -> 1. /. Float.max_num (-0.) (+0.)), infinity));
  (86, Equal((fun () -> 1. /. Float.max_num (+0.) (-0.)), infinity));

  (87, Pair ((fun () -> Float.min_max_num 1. 2.), (1., 2.)));
  (88, Pair ((fun () -> Float.min_max_num 2. 1.), (1., 2.)));
  (89, Pair ((fun () -> Float.min_max_num 1. nan), (1., 1.)));
  (90, Pair ((fun () -> Float.min_max_num nan 1.), (1., 1.)));
  (91, True (fun () -> Float.(is_nan2(min_max_num nan nan))));
  (92, Pair ((fun () -> let x, y = Float.min_max_num (-0.) (+0.) in
                        (1. /. x, 1. /. y)), (neg_infinity, infinity)));
  (93, Pair ((fun () -> let x, y = Float.min_max_num (+0.) (-0.) in
                        (1. /. x, 1. /. y)), (neg_infinity, infinity)));
]

let () =
  let f (n, test) =
    match test with
    | True p ->
        Printf.printf "%03d: %s\n%!" n (if p () then "OK" else "FAIL")
    | False p ->
        Printf.printf "%03d: %s\n%!" n (if p () then "FAIL" else "OK")
    | Equal (f, result) ->
        let v = f () in
        if v = result then
          Printf.printf "%03d: OK\n%!" n
        else
          Printf.printf "%03d: FAIL (%h returned instead of %h)\n%!" n v result
    | Pair (f, ((l', r') as result)) ->
        let (l, r) as v = f () in
        if v = result then
          Printf.printf "%03d: OK\n%!" n
        else
          Printf.printf "%03d: FAIL ((%h, %h) returned instead of (%h, %h))\n%!" n l r l' r'
  in
  List.iter f cases
