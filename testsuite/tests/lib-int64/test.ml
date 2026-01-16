(* TEST *)

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

let test_div () =
  let divzero f x y =
    try ignore (f x y); false with Division_by_zero -> true in
  let check x y =
    if y = 0L then begin
      assert (divzero Int64.div x y);
      assert (divzero Int64.rem x y);
      assert (divzero Int64.fdiv x y);
      assert (divzero Int64.cdiv x y);
      assert (divzero Int64.ediv x y);
      assert (divzero Int64.erem x y)
    end else begin
      let q = Int64.div x y
      and r = Int64.rem x y
      and f = Int64.fdiv x y
      and c = Int64.cdiv x y
      and q' = Int64.ediv x y
      and r' = Int64.erem x y in
      assert (x = Int64.add (Int64.mul q y) r);
      assert (Int64.abs r <= Int64.(sub (abs y) 1L));
      assert (x = Int64.add (Int64.mul q' y) r');
      assert (0L <= r' && r' <= Int64.(sub (abs y) 1L));
      assert (f <= q && q <= c);
      if r = 0L then assert (f = q && q = c);
      assert (q' = (if y > 0L then f else c))
    end in
  for _i = 1 to 1000 do
    check (Random.bits64()) (Random.bits64());
    check (Random.bits64())
          (Random.int64_in_range ~min:(-10000L) ~max:10000L)
  done;
  let interesting_values =
    [Int64.min_int; -119L; -99L; -3L; -2L; -1L; 0L;
     1L; 2L; 3L; 99L; 119L; Int64.max_int] in
  List.iter
    (fun x -> List.iter (check x) interesting_values)
    interesting_values

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

let naive_popcount n =
  let c = ref 0 in
  for i = 0 to 63 do
    if Int64.(logand n (shift_left 1L i)) <> 0L then incr c
  done;
  !c

let in_unsigned_range x nbits =
  if nbits < 0 then false else
  if nbits >= 64 then true else
  0L <= x && x <= Int64.(pred (shift_left 1L nbits))

let in_signed_range x nbits =
  if nbits < 1 then false else
  if nbits >= 64 then true else
  let bound = Int64.shift_left 1L (nbits - 1) in
  Int64.(neg bound <= x && x <= pred bound)

let top_unsigned_bits x nbits =
  if nbits <= 0 then 0L else Int64.(shift_right_logical x (64 - nbits))

let top_signed_bits x nbits =
  if nbits <= 0 then 0L else Int64.(shift_right x (64 - nbits))

let test_bitcounts () =
  let check n =
    let a = Int64.unsigned_bitsize n
    and z = Int64.leading_zeros n
    and b = Int64.signed_bitsize n
    and s = Int64.leading_sign_bits n in
    assert (a + z = 64);
    assert (b + s = 64);
    (* Check 0 <= n < 2^a (unsigned) *)
    assert (in_unsigned_range n a);
    if a > 0 then assert (not (in_unsigned_range n (a - 1)));
    (* Check -2^{b-1} <= n < 2^{b-1} - 1 (signed) *)
    assert (in_signed_range n b);
    if b > 1 then assert (not (in_signed_range n (b - 1)));
    (* Check top z bits are 0 and the next bit is 1 *)
    assert (top_unsigned_bits n z = 0L);
    assert (z = 64 || top_unsigned_bits n (z+1) <> 0L);
    (* Check top s + 1 bits are sign bits and the next bit is not *)
    assert (let x = top_signed_bits n (s+1) in x = 0L || x = -1L);
    assert (s = 63 || let x = top_signed_bits n (s+2) in x <> 0L && x <> -1L);
    (* Check bottom t bits are 0 and the next bit is 1 *)
    let t = Int64.trailing_zeros n in
    if n = 0L then assert (t = 64) else begin
      let m = Int64.(shift_left (-1L) t) in
      assert (Int64.(logand n m) = n);
      assert (Int64.(logand n (shift_left m 1)) <> n)
    end;
    (* Check popcount against naive count *)
    let p = Int64.popcount n in
    assert (p = naive_popcount n) in
  List.iter check
    [0L; 1L; 2L; 3L; -1L; -2L; -3L; Int64.min_int; Int64.max_int];
  for _i = 1 to 1000 do
    check (Int64.shift_left (Random.int64 0xFFL) (Random.int 48));
    check (Random.bits64())
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
