(* TEST
*)

let test_consts () =
  assert (Int64.zero = 0L);
  assert (Int64.one = 1L);
  assert (Int64.minus_one = -1L);
  ()

let test_arith () =
  assert (Int64.add 2L 4L = 6L);
  assert (Int64.sub 6L 2L = 4L);
  assert (Int64.mul 6L 2L = 12L);
  assert (Int64.div 12L 2L = 6L);
  assert (Int64.rem 5L 2L = 1L);
  assert (Int64.succ 5L = 6L);
  assert (Int64.pred 5L = 4L);
  assert (Int64.abs (-5L) = 5L);
  assert (Int64.abs 5L = 5L);
  ()

let test_logops () =
  assert (Int64.logand 0xF0F0L 0xFFFFL = 0xF0F0L);
  assert (Int64.logor 0xF0FFL 0x0F0FL = 0xFFFFL);
  assert (Int64.logxor 0xF0FFL 0x0F0FL = 0xFFF0L);
  assert (Int64.lognot Int64.max_int = Int64.min_int);
  assert (Int64.shift_left 1L 4 = 16L);
  assert (Int64.shift_right 16L 4 = 1L);
  assert (Int64.shift_right (-16L) 4 = (-1L));
  assert (Int64.shift_right (-16L) 4 = (-1L));
  assert (Int64.shift_right_logical Int64.min_int 63 = 1L);
  ()

let test_equal () =
  assert (Int64.equal 1L 1L = true);
  assert (Int64.equal 1L 0L = false);
  ()

let test_compare () =
  assert (Int64.compare 3L 3L = 0);
  assert (Int64.compare 3L 4L = (-1));
  assert (Int64.compare 4L 3L = 1);
  assert (Int64.compare (-4L) 3L = -1);
  assert (Int64.compare 3L (-4L) = 1);
  ()

let test_float_conv () =
  assert (Int64.to_float 5L = 5.0);
  assert (Int64.of_float 5. = 5L);
  assert (Int64.of_float 5.9 = 5L);
  ()

let test_string_conv () =
  assert (Int64.to_string 50L = "50");
(*  assert (Int64.of_string "50" = Some 50);
  assert (Int64.of_string "" = None); *)
  ()

let test_min_max () =
  assert (Int64.max 2L 3L = 3L);
  assert (Int64.min 2L 3L = 2L)

let tests () =
  test_consts ();
  test_arith ();
  test_logops ();
  test_equal ();
  test_compare ();
  test_float_conv ();
  test_string_conv ();
  test_min_max ();
  ()

let () =
  tests ();
  print_endline "OK"
