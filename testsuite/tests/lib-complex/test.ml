(* TEST *)

let approx_equal ~eps a b =
  abs_float (a -. b) < eps

let check_sqrt n z expected_re expected_im =
  let r = Complex.sqrt z in
  let ok =
    approx_equal ~eps:1e-10 r.re expected_re
    && approx_equal ~eps:1e-10 r.im expected_im
    (* Also check sign of zero *)
    && Float.sign_bit r.re = Float.sign_bit expected_re
    && Float.sign_bit r.im = Float.sign_bit expected_im
  in
  Printf.printf "%03d: %s\n" n (if ok then "OK" else
    Printf.sprintf "FAIL: sqrt(%g + %gi) = %g + %gi, expected %g + %gi"
      z.re z.im r.re r.im expected_re expected_im)

let () =
  (* Basic cases *)
  check_sqrt 1 {re = 4.0; im = 0.0} 2.0 0.0;
  check_sqrt 2 {re = 0.0; im = 0.0} 0.0 0.0;
  check_sqrt 3 {re = 1.0; im = 0.0} 1.0 0.0;
  check_sqrt 4 {re = -1.0; im = 0.0} 0.0 1.0;
  (* Negative reals: branch cut with signed zero *)
  check_sqrt 5 {re = -4.0; im = 0.0} 0.0 2.0;
  check_sqrt 6 {re = -4.0; im = -0.0} 0.0 (-2.0);
  check_sqrt 7 {re = -1.0; im = -0.0} 0.0 (-1.0);
  check_sqrt 8 {re = -9.0; im = 0.0} 0.0 3.0;
  check_sqrt 9 {re = -9.0; im = -0.0} 0.0 (-3.0);
  (* Complex values *)
  check_sqrt 10 {re = 0.0; im = 4.0}
    (sqrt 2.0) (sqrt 2.0);
  check_sqrt 11 {re = 0.0; im = -4.0}
    (sqrt 2.0) (-. (sqrt 2.0));
  check_sqrt 12 {re = 3.0; im = 4.0}
    2.0 1.0;
  check_sqrt 13 {re = 3.0; im = -4.0}
    2.0 (-1.0)
