(* TEST *)

let test_consts () =
  assert (Int.zero = 0);
  assert (Int.one = 1);
  assert (Int.minus_one = -1);
  ()

let test_arith () =
  assert (Int.add 2 4 = 6);
  assert (Int.sub 6 2 = 4);
  assert (Int.mul 6 2 = 12);
  assert (Int.div 12 2 = 6);
  assert (Int.rem 5 2 = 1);
  assert (Int.succ 5 = 6);
  assert (Int.pred 5 = 4);
  assert (Int.abs (-5) = 5);
  assert (Int.abs 5 = 5);
  ()

let test_div () =
  let divzero f x y =
    try ignore (f x y); false with Division_by_zero -> true in
  let check x y =
    if y = 0 then begin
      assert (divzero Int.div x y);
      assert (divzero Int.rem x y);
      assert (divzero Int.fdiv x y);
      assert (divzero Int.cdiv x y);
      assert (divzero Int.ediv x y);
      assert (divzero Int.erem x y)
    end else begin
      let q = Int.div x y
      and r = Int.rem x y
      and f = Int.fdiv x y
      and c = Int.cdiv x y
      and q' = Int.ediv x y
      and r' = Int.erem x y in
      assert (x = Int.add (Int.mul q y) r);
      assert (Int.abs r <= Int.abs y - 1);
      assert (x = Int.add (Int.mul q' y) r');
      assert (0 <= r' && r' <= Int.abs y - 1);
      assert (f <= q && q <= c);
      if r = 0 then assert (f = q && q = c);
      assert (q' = (if y > 0 then f else c))
    end in
  for _i = 1 to 1000 do
    check (Random.int_in_range ~min:Int.min_int ~max:Int.max_int)
          (Random.int_in_range ~min:Int.min_int ~max:Int.max_int);
    check (Random.int_in_range ~min:Int.min_int ~max:Int.max_int)
          (Random.int_in_range ~min:(-10000) ~max:10000)
  done;
  let interesting_values =
    [Int.min_int; -119; -99; -3; -2; -1; 0; 1; 2; 3; 99; 119; Int.max_int] in
  List.iter
    (fun x -> List.iter (check x) interesting_values)
    interesting_values

let test_logops () =
  assert (Int.logand 0xF0F0 0xFFFF = 0xF0F0);
  assert (Int.logor 0xF0FF 0x0F0F = 0xFFFF);
  assert (Int.logxor 0xF0FF 0x0F0F = 0xFFF0);
  assert (Int.lognot Int.max_int = Int.min_int);
  assert (Int.shift_left 1 4 = 16);
  assert (Int.shift_left (Int.compare 0 0) 63 = 0); (* Issue #8864 *)
  assert (Int.shift_right 16 4 = 1);
  assert (Int.shift_right (-16) 4 = (-1));
  assert (Int.shift_right (-16) 4 = (-1));
  assert (Int.shift_right_logical Int.min_int (Sys.int_size - 1) = 1);
  ()

let test_equal () =
  assert (Int.equal 1 1 = true);
  assert (Int.equal 1 0 = false);
  ()

let test_compare () =
  assert (Int.compare 3 3 = 0);
  assert (Int.compare 3 4 = (-1));
  assert (Int.compare 4 3 = 1);
  assert (Int.compare (-4) 3 = -1);
  assert (Int.compare 3 (-4) = 1);
  ()

let test_float_conv () =
  assert (Int.to_float 5 = 5.0);
  assert (Int.of_float 5. = 5);
  assert (Int.of_float 5.9 = 5);
  ()

let test_string_conv () =
  assert (Int.to_string 50 = "50");
(*  assert (Int.of_string "50" = Some 50);
  assert (Int.of_string "" = None); *)
  ()

let test_min_max () =
  assert (Int.max 2 3 = 3);
  assert (Int.min 2 3 = 2)

let test_hash () =
  let f n =
    assert (Hashtbl.hash n = Int.hash n);
    assert (Hashtbl.seeded_hash 16 n = Int.seeded_hash 16 n)
  in
  f 0; f 123; f (-456); f 0x3FFFFFFF; f (-0x40000000)

let tests () =
  test_consts ();
  test_arith ();
  test_div ();
  test_logops ();
  test_equal ();
  test_compare ();
  test_float_conv ();
  test_string_conv ();
  test_min_max ();
  test_hash ();
  ()

let () =
  tests ();
  print_endline "OK"
