(* TEST *)

(* Conversions between floats and boxed integers, in range only. *)

let opaque = Sys.opaque_identity

let check_int name got want =
  if got <> want then
    failwith (Printf.sprintf "%s: got %Ld, expected %Ld" name got want)

let check_float name got want =
  if Int64.bits_of_float got <> Int64.bits_of_float want then
    failwith (Printf.sprintf "%s: got %h, expected %h" name got want)

let () =
  let i64 x = Int64.of_float (opaque x) in
  check_int "i64 0."     (i64 0.)      0L;
  check_int "i64 -0."    (i64 (-0.))   0L;
  check_int "i64 1.5"    (i64 1.5)     1L;
  check_int "i64 -1.5"   (i64 (-1.5))  (-1L);
  check_int "i64 0.999"  (i64 0.999)   0L;
  check_int "i64 1e15"   (i64 1e15)    1_000_000_000_000_000L;
  check_int "i64 2^62"   (i64 0x1p62)  0x4000_0000_0000_0000L;
  check_int "i64 -2^63"  (i64 (-0x1p63)) Int64.min_int;

  let i32 x = Int64.of_int32 (Int32.of_float (opaque x)) in
  check_int "i32 1.5"    (i32 1.5)     1L;
  check_int "i32 -1.5"   (i32 (-1.5))  (-1L);
  check_int "i32 max"    (i32 2147483647.)    2147483647L;
  check_int "i32 min"    (i32 (-2147483648.)) (-2147483648L);

  let nat x = Int64.of_nativeint (Nativeint.of_float (opaque x)) in
  check_int "nat -1.5"   (nat (-1.5))  (-1L);
  check_int "nat 2^30"   (nat 0x1p30)  0x4000_0000L;

  let f64 x = Int64.to_float (opaque x) in
  check_float "f64 0"       (f64 0L)     0.;
  check_float "f64 -1"      (f64 (-1L))  (-1.);
  check_float "f64 max"     (f64 Int64.max_int)  0x1p63;
  check_float "f64 min"     (f64 Int64.min_int)  (-0x1p63);
  (* Halfway, so it rounds to even. *)
  check_float "f64 2^53+1"  (f64 0x20_0000_0000_0001L)  0x1p53;

  let f32 x = Int32.to_float (opaque x) in
  check_float "f32 max"     (f32 Int32.max_int)  2147483647.;
  check_float "f32 min"     (f32 Int32.min_int)  (-2147483648.);
  check_float "f32 -1"      (f32 (-1l))  (-1.);
  (* Only the low 32 bits count. *)
  check_float "f32 add wraps"
    (Int32.to_float (Int32.add (opaque Int32.max_int) 1l))  (-2147483648.);
  check_float "f32 sub wraps"
    (Int32.to_float (Int32.sub (opaque Int32.min_int) 1l))  2147483647.;
  check_float "f32 mul wraps"
    (Int32.to_float (Int32.mul (opaque 0x10000l) 0x10000l))  0.;

  let fnat x = Nativeint.to_float (opaque x) in
  check_float "fnat -1"     (fnat (-1n))   (-1.);
  check_float "fnat 2^30"   (fnat 0x4000_0000n)  0x1p30
