(* TEST
 * bytecode
 * native
*)

external int_popcnt : int -> int = "%popcntint"
external int32_popcnt : int32 -> int = "%int32_popcnt"
external int64_popcnt : int64 -> int = "%int64_popcnt"
external nativeint_popcnt : nativeint -> int = "%nativeint_popcnt"

let bitwidth = Sys.word_size

let test_int () =
  (* call directly *)
  let popcnt = int_popcnt in begin
    assert (popcnt 0 = 0);
    assert (popcnt 1 = 1);
    assert (popcnt 7 = 3);
    assert ((popcnt Int.max_int) = bitwidth-1-1);
    assert ((popcnt Int.min_int) = 1);
    assert ((popcnt (-1)) = bitwidth-1)
  end;
  (* call through standard library routines *)
  let popcnt = Int.count_set_bits in begin
    assert (popcnt 0 = 0);
    assert (popcnt 1 = 1);
    assert (popcnt 7 = 3);
    assert ((popcnt Int.max_int) = bitwidth-1-1);
    assert ((popcnt Int.min_int) = 1);
    assert ((popcnt (-1)) = bitwidth-1)
  end;
  ()

let test_nativeint ()  =
  (* call directly *)
  let popcnt = nativeint_popcnt in begin
    assert (popcnt 0n = 0);
    assert (popcnt 7n = 3);
    assert (popcnt Nativeint.max_int = (bitwidth - 1));
    assert (popcnt Nativeint.min_int = 1);
    assert ((popcnt (-1n)) = bitwidth)
  end;
  (* call through standard library routines *)
  let popcnt = Nativeint.count_set_bits in begin
    assert (popcnt 0n = 0);
    assert (popcnt 7n = 3);
    assert (popcnt Nativeint.max_int = (bitwidth - 1));
    assert (popcnt Nativeint.min_int = 1);
    assert ((popcnt (-1n)) = bitwidth)
  end;
  ()

let test_int32 () =
  (* call directly *)
  let popcnt = int32_popcnt in begin
    assert (popcnt 0l = 0);
    assert (popcnt 7l = 3);
    assert (popcnt Int32.max_int = (32 - 1));
    assert (popcnt Int32.min_int = 1);
    assert ((popcnt (-1l)) = 32)
  end;
  (* call through standard library routines *)
  let popcnt = Int32.count_set_bits in begin
    assert (popcnt 0l = 0);
    assert (popcnt 7l = 3);
    assert (popcnt Int32.max_int = (32 - 1));
    assert (popcnt Int32.min_int = 1);
    assert ((popcnt (-1l)) = 32)
  end;
  ()

let test_int64 () =
  (* call directly *)
  let popcnt = int64_popcnt in begin
    assert (popcnt 0L = 0);
    assert (popcnt 7L = 3);
    assert (popcnt Int64.max_int = (64 - 1));
    assert (popcnt Int64.min_int = 1);
    assert ((popcnt (-1L)) = 64);
  end;
  (* call through standard library routines *)
  let popcnt = Int64.count_set_bits in begin
    assert (popcnt 0L = 0);
    assert (popcnt 7L = 3);
    assert (popcnt Int64.max_int = (64 - 1));
    assert (popcnt Int64.min_int = 1);
    assert ((popcnt (-1L)) = 64);
  end;
  ()

let () =
  test_int ();
  test_nativeint ();
  test_int32 ();
  test_int64 ();
  ()
