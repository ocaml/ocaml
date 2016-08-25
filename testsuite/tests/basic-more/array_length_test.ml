
let identity x = Sys.opaque_identity x
  [@@inline never]

let a0 = Array.make 0 0
let a3 = Array.make 3 3
let a5 = Array.make 5 5
let a7 = Array.make 7 7

let test_compare comparison =
  let [@inline never] test_0 a =
    assert(
      (comparison (Array.length a) 0) =
      (comparison (Array.length a) (identity 0)));
    assert(
      (comparison 0 (Array.length a)) =
      (comparison (identity 0) (Array.length a)))
  in

  let [@inline never] test_5 a =
    assert(
      (comparison (Array.length a) 5) =
      (comparison (Array.length a) (identity 5)));
    assert(
      (comparison 5 (Array.length a)) =
      (comparison (identity 5) (Array.length a)))
  in

  let [@inline never] test_min_5 a =
    assert(
      (comparison (Array.length a) (-5)) =
      (comparison (Array.length a) (identity (-5))));
    assert(
      (comparison (-5) (Array.length a)) =
      (comparison (identity (-5)) (Array.length a)))
  in

  test_0 a0;
  test_0 a3;
  test_0 a5;
  test_0 a7;

  test_5 a0;
  test_5 a3;
  test_5 a5;
  test_5 a7;

  test_min_5 a0;
  test_min_5 a3;
  test_min_5 a5;
  test_min_5 a7
  [@@inline]

let () =
  test_compare (=);
  test_compare (<);
  test_compare (>);
  test_compare (<=);
  test_compare (>=);
  print_endline "OK"

