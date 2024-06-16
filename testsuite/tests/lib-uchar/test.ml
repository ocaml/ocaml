(* TEST *)

let assert_raise_invalid_argument f v =
  assert (try ignore (f v); false with Invalid_argument _ -> true)

let test_constants () =
  assert (Uchar.(to_int min) = 0x0000);
  assert (Uchar.(to_int max) = 0x10FFFF);
  assert (Uchar.(to_int bom) = 0xFEFF);
  assert (Uchar.(to_int rep) = 0xFFFD);
  ()

let test_succ () =
  assert (Uchar.(to_int (succ min)) = 0x0001);
  assert (Uchar.(to_int (succ (of_int 0xD7FF))) = 0xE000);
  assert (Uchar.(to_int (succ (of_int 0xE000))) = 0xE001);
  assert_raise_invalid_argument Uchar.succ Uchar.max;
  ()

let test_pred () =
  assert_raise_invalid_argument Uchar.pred Uchar.min;
  assert (Uchar.(to_int (pred (of_int 0xD7FF))) = 0xD7FE);
  assert (Uchar.(to_int (pred (of_int 0xE000))) = 0xD7FF);
  assert (Uchar.(to_int (pred max)) = 0x10FFFE);
  ()

let test_is_valid () =
  assert (not (Uchar.is_valid (-1)));
  assert (Uchar.is_valid 0x0000);
  assert (Uchar.is_valid 0xD7FF);
  assert (not (Uchar.is_valid 0xD800));
  assert (not (Uchar.is_valid 0xDFFF));
  assert (Uchar.is_valid 0xE000);
  assert (Uchar.is_valid 0x10FFFF);
  assert (not (Uchar.is_valid 0x110000));
  assert (not (Uchar.is_valid min_int));
  assert (not (Uchar.is_valid max_int));
  ()

let char_max = Uchar.of_int 0x00FF

let test_is_char () =
  assert (Uchar.(is_char Uchar.min));
  assert (Uchar.(is_char char_max));
  assert (Uchar.(not (is_char (of_int 0x0100))));
  assert (not (Uchar.is_char Uchar.max));
  ()

let test_of_char () =
  assert (Uchar.(equal (of_char '\xFF') char_max));
  assert (Uchar.(equal (of_char '\x00') min));
  ()

let test_to_char () =
  assert (Uchar.(to_char min) = '\x00');
  assert (Uchar.(to_char char_max) = '\xFF');
  assert_raise_invalid_argument Uchar.to_char (Uchar.succ char_max);
  assert_raise_invalid_argument Uchar.to_char Uchar.max;
  ()

let test_equal () =
  assert (Uchar.(equal min min));
  assert (Uchar.(equal max max));
  assert (not Uchar.(equal min max));
  ()

let test_compare () =
  assert (Uchar.(compare min min) = 0);
  assert (Uchar.(compare max max) = 0);
  assert (Uchar.(compare min max) = (-1));
  assert (Uchar.(compare max min) = 1);
  ()

let test_hash () =
  let f u =
    assert (Hashtbl.hash u = Uchar.hash u);
    assert (Hashtbl.seeded_hash 42 u = Uchar.seeded_hash 42 u)
  in
  List.iter (Fun.compose f Uchar.of_int)
    [0x0000; 0x002D; 0x00E9; 0x062D; 0x2014; 0x1F349]

let test_utf_decode () =
  let d0 = Uchar.utf_decode 1 Uchar.min in
  let d1 = Uchar.utf_decode 4 Uchar.max in
  let invalid = Uchar.utf_decode_invalid 3 in
  assert (Uchar.utf_decode_is_valid d0);
  assert (Uchar.utf_decode_length d0 = 1);
  assert (Uchar.equal (Uchar.utf_decode_uchar d0) Uchar.min);
  assert (Uchar.utf_decode_is_valid d1);
  assert (Uchar.utf_decode_length d1 = 4);
  assert (Uchar.equal (Uchar.utf_decode_uchar d1) Uchar.max);
  assert (not (Uchar.utf_decode_is_valid invalid));
  assert (Uchar.utf_decode_length invalid = 3);
  assert (Uchar.equal (Uchar.utf_decode_uchar invalid) Uchar.rep);
  ()

let test_utf_x_byte_length () =
  assert (Uchar.utf_8_byte_length Uchar.min = 1);
  assert (Uchar.utf_16_byte_length Uchar.min = 2);
  assert (Uchar.utf_8_byte_length Uchar.max = 4);
  assert (Uchar.utf_16_byte_length Uchar.max = 4);
  let c = Uchar.of_int 0x1F42B in
  assert (Uchar.utf_8_byte_length c = 4);
  assert (Uchar.utf_16_byte_length c = 4);
  let c = Uchar.of_int 0x9A7C in
  assert (Uchar.utf_8_byte_length c = 3);
  assert (Uchar.utf_16_byte_length c = 2);
  ()

let tests () =
  test_constants ();
  test_succ ();
  test_pred ();
  test_is_valid ();
  test_is_char ();
  test_of_char ();
  test_to_char ();
  test_equal ();
  test_compare ();
  test_hash ();
  test_utf_decode ();
  test_utf_x_byte_length ();
  ()

let () =
  tests ();
  print_endline "OK"
