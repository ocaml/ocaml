(* TEST *)

let test_consts () =
  assert (Int32.zero = 0l);
  assert (Int32.one = 1l);
  assert (Int32.minus_one = -1l);
  ()

let test_arith () =
  assert (Int32.add 2l 4l = 6l);
  assert (Int32.sub 6l 2l = 4l);
  assert (Int32.mul 6l 2l = 12l);
  assert (Int32.div 12l 2l = 6l);
  assert (Int32.rem 5l 2l = 1l);
  assert (Int32.succ 5l = 6l);
  assert (Int32.pred 5l = 4l);
  assert (Int32.abs (-5l) = 5l);
  assert (Int32.abs 5l = 5l);
  ()

let test_div () =
  let divzero f x y =
    try ignore (f x y); false with Division_by_zero -> true in
  let check x y =
    if y = 0l then begin
      assert (divzero Int32.div x y);
      assert (divzero Int32.rem x y);
      assert (divzero Int32.fdiv x y);
      assert (divzero Int32.cdiv x y);
      assert (divzero Int32.ediv x y);
      assert (divzero Int32.erem x y)
    end else begin
      let q = Int32.div x y
      and r = Int32.rem x y
      and f = Int32.fdiv x y
      and c = Int32.cdiv x y
      and q' = Int32.ediv x y
      and r' = Int32.erem x y in
      assert (x = Int32.add (Int32.mul q y) r);
      assert (Int32.abs r <= Int32.(sub (abs y) 1l));
      assert (x = Int32.add (Int32.mul q' y) r');
      assert (0l <= r' && r' <= Int32.(sub (abs y) 1l));
      assert (f <= q && q <= c);
      if r = 0l then assert (f = q && q = c);
      assert (q' = (if y > 0l then f else c))
    end in
  for _i = 1 to 1000 do
    check (Random.bits32()) (Random.bits32());
    check (Random.bits32())
          (Random.int32_in_range ~min:(-10000l) ~max:10000l)
  done;
  let interesting_values =
    [Int32.min_int; -119l; -99l; -3l; -2l; -1l; 0l;
     1l; 2l; 3l; 99l; 119l; Int32.max_int] in
  List.iter
    (fun x -> List.iter (check x) interesting_values)
    interesting_values

let test_logops () =
  assert (Int32.logand 0xF0F0l 0xFFFFl = 0xF0F0l);
  assert (Int32.logor 0xF0FFl 0x0F0Fl = 0xFFFFl);
  assert (Int32.logxor 0xF0FFl 0x0F0Fl = 0xFFF0l);
  assert (Int32.lognot Int32.max_int = Int32.min_int);
  assert (Int32.shift_left 1l 4 = 16l);
  assert (Int32.shift_right 16l 4 = 1l);
  assert (Int32.shift_right (-16l) 4 = (-1l));
  assert (Int32.shift_right (-16l) 4 = (-1l));
  assert (Int32.shift_right_logical Int32.min_int 31 = 1l);
  ()

let test_equal () =
  assert (Int32.equal 1l 1l = true);
  assert (Int32.equal 1l 0l = false);
  ()

let test_compare () =
  assert (Int32.compare 3l 3l = 0);
  assert (Int32.compare 3l 4l = (-1));
  assert (Int32.compare 4l 3l = 1);
  assert (Int32.compare (-4l) 3l = -1);
  assert (Int32.compare 3l (-4l) = 1);
  ()

let test_float_conv () =
  assert (Int32.to_float 5l = 5.0);
  assert (Int32.of_float 5. = 5l);
  assert (Int32.of_float 5.9 = 5l);
  ()

let test_string_conv () =
  assert (Int32.to_string 50l = "50");
  ()

let test_min_max () =
  assert (Int32.max 2l 3l = 3l);
  assert (Int32.min 2l 3l = 2l)

let naive_popcount n =
  let c = ref 0 in
  for i = 0 to 31 do
    if Int32.(logand n (shift_left 1l i)) <> 0l then incr c
  done;
  !c

let in_unsigned_range x nbits =
  if nbits < 0 then false else
  if nbits >= 32 then true else
  0l <= x && x <= Int32.(pred (shift_left 1l nbits))

let in_signed_range x nbits =
  if nbits < 1 then false else
  if nbits >= 32 then true else
  let bound = Int32.shift_left 1l (nbits - 1) in
  Int32.(neg bound <= x && x <= pred bound)

let top_unsigned_bits x nbits =
  if nbits <= 0 then 0l else Int32.(shift_right_logical x (32 - nbits))

let top_signed_bits x nbits =
  if nbits <= 0 then 0l else Int32.(shift_right x (32 - nbits))

let test_bitcounts () =
  let check n =
    let a = Int32.unsigned_bitsize n
    and z = Int32.leading_zeros n
    and b = Int32.signed_bitsize n
    and s = Int32.leading_sign_bits n in
    assert (a + z = 32);
    assert (b + s = 32);
    (* Check 0 <= n < 2^a (unsigned) *)
    assert (in_unsigned_range n a);
    if a > 0 then assert (not (in_unsigned_range n (a - 1)));
    (* Check -2^{b-1} <= n < 2^{b-1} - 1 (signed) *)
    assert (in_signed_range n b);
    if b > 1 then assert (not (in_signed_range n (b - 1)));
    (* Check top z bits are 0 and the next bit is 1 *)
    assert (top_unsigned_bits n z = 0l);
    assert (z = 32 || top_unsigned_bits n (z+1) <> 0l);
    (* Check top s + 1 bits are sign bits and the next bit is not *)
    assert (let x = top_signed_bits n (s+1) in x = 0l || x = -1l);
    assert (s = 31 || let x = top_signed_bits n (s+2) in x <> 0l && x <> -1l);
    (* Check bottom t bits are 0 and the next bit is 1 *)
    let t = Int32.trailing_zeros n in
    if n = 0l then assert (t = 32) else begin
      let m = Int32.(shift_left (-1l) t) in
      assert (Int32.(logand n m) = n);
      assert (Int32.(logand n (shift_left m 1)) <> n)
    end;
    (* Check popcount against naive count *)
    let p = Int32.popcount n in
    assert (p = naive_popcount n) in
  List.iter check
    [0l; 1l; 2l; 3l; -1l; -2l; -3l; Int32.min_int; Int32.max_int];
  for _i = 1 to 1000 do
    check (Int32.shift_left (Random.int32 0xFFl) (Random.int 16));
    check (Random.bits32())
  done

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
  ()

let () =
  tests ();
  print_endline "OK"
