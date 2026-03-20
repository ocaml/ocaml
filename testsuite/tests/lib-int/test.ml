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

let naive_popcount n =
  let c = ref 0 in
  for i = 0 to Sys.int_size do
    if Int.(logand n (shift_left 1 i)) <> 0 then incr c
  done;
  !c

let in_unsigned_range x nbits =
  if nbits < 0 then false else
  if nbits >= Sys.int_size then true else
  0 <= x && x <= Int.(pred (shift_left 1 nbits))

let in_signed_range x nbits =
  if nbits < 1 then false else
  if nbits >= Sys.int_size then true else
  let bound = Int.shift_left 1 (nbits - 1) in
  Int.(neg bound <= x && x <= pred bound)

let top_unsigned_bits x nbits =
  if nbits <= 0 then 0 else Int.(shift_right_logical x (Sys.int_size - nbits))

let top_signed_bits x nbits =
  if nbits <= 0 then 0 else Int.(shift_right x (Sys.int_size - nbits))

let test_bitcounts () =
  let check n =
    let a = Int.unsigned_bitsize n
    and z = Int.leading_zeros n
    and b = Int.signed_bitsize n
    and s = Int.leading_sign_bits n in
    assert (a + z = Sys.int_size);
    assert (b + s = Sys.int_size);
    (* Check 0 <= n < 2^a (unsigned) *)
    assert (in_unsigned_range n a);
    if a > 0 then assert (not (in_unsigned_range n (a - 1)));
    (* Check -2^{b-1} <= n < 2^{b-1} - 1 (signed) *)
    assert (in_signed_range n b);
    if b > 1 then assert (not (in_signed_range n (b - 1)));
    (* Check top z bits are 0 and the next bit is 1 *)
    assert (top_unsigned_bits n z = 0);
    assert (z = Sys.int_size || top_unsigned_bits n (z+1) <> 0);
    (* Check top s + 1 bits are sign bits and the next bit is not *)
    assert (let x = top_signed_bits n (s+1) in x = 0 || x = -1);
    assert (s = Sys.int_size - 1
            || let x = top_signed_bits n (s+2) in x <> 0 && x <> -1);
    (* Check bottom t bits are 0 and the next bit is 1 *)
    let t = Int.trailing_zeros n in
    if n = 0 then assert (t = Sys.int_size) else begin
      let m = Int.(shift_left (-1) t) in
      assert (Int.(logand n m) = n);
      assert (Int.(logand n (shift_left m 1)) <> n)
    end;
    (* Check popcount against naive count *)
    let p = Int.popcount n in
    assert (p = naive_popcount n) in
  List.iter check
    [0; 1; 2; 3; -1; -2; -3; Int.min_int; Int.max_int];
  for _i = 1 to 1000 do
    check (Int.shift_left (Random.int 0xFF) (Random.int (Sys.int_size - 8)));
    check (Random.int_in_range ~min: Int.min_int ~max: Int.max_int)
  done

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
  test_bitcounts ();
  test_hash ();
  ()

let () =
  tests ();
  print_endline "OK"
