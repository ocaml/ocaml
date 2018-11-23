(* TEST
 * bytecode
 * native
*)

external int_clz : int -> int = "%clzint"
external int32_clz : int32 -> int = "%int32_clz"
external int64_clz : int64 -> int = "%int64_clz"
external nativeint_clz : nativeint -> int = "%nativeint_clz"

let bitwidth = Sys.word_size

let test_int () =
  (* call directly *)
  assert (int_clz 0 = bitwidth-1);
  assert (int_clz 1 = (bitwidth - 1 - 1));
  assert (int_clz 7 = (bitwidth - 1 - 3));
  assert ((int_clz Int.max_int) = 1);
  assert ((int_clz Int.min_int) = 0);
  (* call through standard library routines *)
  assert (Int.count_leading_zeros 0 = bitwidth-1);
  assert (Int.count_leading_zeros 1 = (bitwidth - 1 - 1));
  assert (Int.count_leading_zeros 7 = (bitwidth - 1 - 3));
  assert ((Int.count_leading_zeros Int.max_int) = 1);
  assert ((Int.count_leading_zeros Int.min_int) = 0);
  ()

let test_nativeint () =
   (* call directly *)
  assert (nativeint_clz 0n = bitwidth);
  assert (nativeint_clz 7n = (bitwidth-3));
  assert (nativeint_clz Nativeint.max_int = 1);
  assert (nativeint_clz Nativeint.min_int = 0);
  assert (nativeint_clz (-1n) = 0);
  (* call through standard library routines *)
  assert (Nativeint.count_leading_zeros 0n = bitwidth);
  assert (Nativeint.count_leading_zeros 7n = (bitwidth-3));
  assert (Nativeint.count_leading_zeros Nativeint.max_int = 1);
  assert (Nativeint.count_leading_zeros Nativeint.min_int = 0);
  assert (Nativeint.count_leading_zeros (-1n) = 0);
  ()

let test_int32 () =
  (* call directly *)
  assert (int32_clz 0l = 32);
  assert (int32_clz 7l = (32-3));
  assert (int32_clz Int32.max_int = 1);
  assert (int32_clz (-1l) = 0);
  (* call through standard library routines *)
  assert (Int32.count_leading_zeros 0l = 32);
  assert (Int32.count_leading_zeros 7l = (32-3));
  assert (Int32.count_leading_zeros Int32.max_int = 1);
  assert (Int32.count_leading_zeros (-1l) = 0);
  ()

let test_int64 () =
  (* call directly *)
  assert (int64_clz 0L = 64);
  assert (int64_clz 7L = (64-3));
  assert (int64_clz (-1L) = 0);
  (* call through standard library routines *)
  assert (Int64.count_leading_zeros 0L = 64);
  assert (Int64.count_leading_zeros 7L = (64-3));
  assert (Int64.count_leading_zeros (-1L) = 0);
  ()

let () =
  test_int ();
  test_nativeint ();
  test_int32 ();
  test_int64 ();
  ()
