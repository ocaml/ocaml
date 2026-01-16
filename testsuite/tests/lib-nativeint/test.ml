(* TEST *)

let test_consts () =
  assert (Nativeint.zero = 0n);
  assert (Nativeint.one = 1n);
  assert (Nativeint.minus_one = -1n);
  ()

let test_arith () =
  assert (Nativeint.add 2n 4n = 6n);
  assert (Nativeint.sub 6n 2n = 4n);
  assert (Nativeint.mul 6n 2n = 12n);
  assert (Nativeint.div 12n 2n = 6n);
  assert (Nativeint.rem 5n 2n = 1n);
  assert (Nativeint.succ 5n = 6n);
  assert (Nativeint.pred 5n = 4n);
  assert (Nativeint.abs (-5n) = 5n);
  assert (Nativeint.abs 5n = 5n);
  ()

let test_div () =
  let divzero f x y =
    try ignore (f x y); false with Division_by_zero -> true in
  let check x y =
    if y = 0n then begin
      assert (divzero Nativeint.div x y);
      assert (divzero Nativeint.rem x y);
      assert (divzero Nativeint.fdiv x y);
      assert (divzero Nativeint.cdiv x y);
      assert (divzero Nativeint.ediv x y);
      assert (divzero Nativeint.erem x y)
    end else begin
      let q = Nativeint.div x y
      and r = Nativeint.rem x y
      and f = Nativeint.fdiv x y
      and c = Nativeint.cdiv x y
      and q' = Nativeint.ediv x y
      and r' = Nativeint.erem x y in
      assert (x = Nativeint.add (Nativeint.mul q y) r);
      assert (Nativeint.abs r <= Nativeint.(sub (abs y) 1n));
      assert (x = Nativeint.add (Nativeint.mul q' y) r');
      assert (0n <= r' && r' <= Nativeint.(sub (abs y) 1n));
      assert (f <= q && q <= c);
      if r = 0n then assert (f = q && q = c);
      assert (q' = (if y > 0n then f else c))
    end in
  for _i = 1 to 1000 do
    check (Random.nativebits()) (Random.nativebits());
    check (Random.nativebits())
          (Random.nativeint_in_range ~min:(-10000n) ~max:10000n)
  done;
  let interesting_values =
    [Nativeint.min_int; -119n; -99n; -3n; -2n; -1n; 0n;
     1n; 2n; 3n; 99n; 119n; Nativeint.max_int] in
  List.iter
    (fun x -> List.iter (check x) interesting_values)
    interesting_values

let test_logops () =
  assert (Nativeint.logand 0xF0F0n 0xFFFFn = 0xF0F0n);
  assert (Nativeint.logor 0xF0FFn 0x0F0Fn = 0xFFFFn);
  assert (Nativeint.logxor 0xF0FFn 0x0F0Fn = 0xFFF0n);
  assert (Nativeint.lognot Nativeint.max_int = Nativeint.min_int);
  assert (Nativeint.shift_left 1n 4 = 16n);
  assert (Nativeint.shift_right 16n 4 = 1n);
  assert (Nativeint.shift_right (-16n) 4 = (-1n));
  assert (Nativeint.shift_right (-16n) 4 = (-1n));
  assert (Nativeint.(shift_right_logical min_int (size - 1)) = 1n);
  ()

let test_equal () =
  assert (Nativeint.equal 1n 1n = true);
  assert (Nativeint.equal 1n 0n = false);
  ()

let test_compare () =
  assert (Nativeint.compare 3n 3n = 0);
  assert (Nativeint.compare 3n 4n = (-1));
  assert (Nativeint.compare 4n 3n = 1);
  assert (Nativeint.compare (-4n) 3n = -1);
  assert (Nativeint.compare 3n (-4n) = 1);
  ()

let test_float_conv () =
  assert (Nativeint.to_float 5n = 5.0);
  assert (Nativeint.of_float 5. = 5n);
  assert (Nativeint.of_float 5.9 = 5n);
  ()

let test_string_conv () =
  assert (Nativeint.to_string 50n = "50");
  ()

let test_min_max () =
  assert (Nativeint.max 2n 3n = 3n);
  assert (Nativeint.min 2n 3n = 2n)

let naive_popcount n =
  let c = ref 0 in
  for i = 0 to Nativeint.size - 1 do
    if Nativeint.(logand n (shift_left 1n i)) <> 0n then incr c
  done;
  !c

let in_unsigned_range x nbits =
  if nbits < 0 then false else
  if nbits >= Nativeint.size then true else
  0n <= x && x <= Nativeint.(pred (shift_left 1n nbits))

let in_signed_range x nbits =
  if nbits < 1 then false else
  if nbits >= Nativeint.size then true else
  let bound = Nativeint.shift_left 1n (nbits - 1) in
  Nativeint.(neg bound <= x && x <= pred bound)

let top_unsigned_bits x nbits =
  if nbits <= 0 then 0n else Nativeint.(shift_right_logical x (size - nbits))

let top_signed_bits x nbits =
  if nbits <= 0 then 0n else Nativeint.(shift_right x (size - nbits))

let test_bitcounts () =
  let check n =
    let a = Nativeint.unsigned_bitsize n
    and z = Nativeint.leading_zeros n
    and b = Nativeint.signed_bitsize n
    and s = Nativeint.leading_sign_bits n in
    assert (a + z = Nativeint.size);
    assert (b + s = Nativeint.size);
    (* Check 0 <= n < 2^a (unsigned) *)
    assert (in_unsigned_range n a);
    if a > 0 then assert (not (in_unsigned_range n (a - 1)));
    (* Check -2^{b-1} <= n < 2^{b-1} - 1 (signed) *)
    assert (in_signed_range n b);
    if b > 1 then assert (not (in_signed_range n (b - 1)));
    (* Check top z bits are 0 and the next bit is 1 *)
    assert (top_unsigned_bits n z = 0n);
    assert (z = Nativeint.size || top_unsigned_bits n (z+1) <> 0n);
    (* Check top s + 1 bits are sign bits and the next bit is not *)
    assert (let x = top_signed_bits n (s+1) in x = 0n || x = -1n);
    assert (s = Nativeint.size - 1
            || let x = top_signed_bits n (s+2) in x <> 0n && x <> -1n);
    (* Check bottom t bits are 0 and the next bit is 1 *)
    let t = Nativeint.trailing_zeros n in
    if n = 0n then assert (t = Nativeint.size) else begin
      let m = Nativeint.(shift_left (-1n) t) in
      assert (Nativeint.(logand n m) = n);
      assert (Nativeint.(logand n (shift_left m 1)) <> n)
    end;
    (* Check popcount against naive count *)
    let p = Nativeint.popcount n in
    assert (p = naive_popcount n) in
  List.iter check
    [0n; 1n; 2n; 3n; -1n; -2n; -3n; Nativeint.min_int; Nativeint.max_int];
  for _i = 1 to 1000 do
    check (Nativeint.shift_left
             (Random.nativeint 0xFFn)
             (Random.int (Nativeint.size - 16)));
    check (Random.nativebits())
  done

let tests () =
  test_consts ();
  test_arith ();
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
