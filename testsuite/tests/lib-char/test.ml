(* TEST *)

let assert_raise_invalid_argument f v =
  assert (try ignore (f v); false with Invalid_argument _ -> true)

let test_constants () =
  assert (Char.equal Char.Ascii.min '\x00');
  assert (Char.equal Char.Ascii.max '\x7F');
  ()

let test_is_valid () =
  assert (not (Char.Ascii.is_valid '\x80'));
  assert (Char.Ascii.is_valid '\x00');
  assert (Char.Ascii.is_valid '\x7F');
  assert (Char.Ascii.is_valid 'a');
  ()

let test_preds () =
  assert (Char.Ascii.is_upper 'A');
  assert (not (Char.Ascii.is_upper 'z'));
  assert (Char.Ascii.is_lower 'z');
  assert (not (Char.Ascii.is_lower 'A'));
  assert (Char.Ascii.is_letter 'A');
  assert (Char.Ascii.is_letter 'z');
  assert (not (Char.Ascii.is_letter '1'));
  assert (Char.Ascii.is_alphanum 'A');
  assert (Char.Ascii.is_alphanum 'z');
  assert (Char.Ascii.is_alphanum '1');
  assert (not (Char.Ascii.is_alphanum '.'));
  assert (Char.Ascii.is_white ' ');
  assert (not (Char.Ascii.is_white 'a'));
  assert (Char.Ascii.is_blank ' ');
  assert (not (Char.Ascii.is_blank '\n'));
  assert (Char.Ascii.is_graphic 'a');
  assert (Char.Ascii.is_graphic '.');
  assert (not (Char.Ascii.is_graphic ' '));
  assert (Char.Ascii.is_print 'a');
  assert (Char.Ascii.is_print '.');
  assert (Char.Ascii.is_print ' ');
  assert (not (Char.Ascii.is_print '\n'));
  assert (Char.Ascii.is_control '\n');
  assert (Char.Ascii.is_control '\x7F');
  assert (not (Char.Ascii.is_control ' '));
  ()

let test_decimal () =
  assert (Char.Ascii.is_digit '0');
  assert (not (Char.Ascii.is_digit 'A'));
  assert (Char.Ascii.digit_to_int '5' = 5);
  assert (Char.Ascii.digit_to_int '9' = 9);
  assert_raise_invalid_argument Char.Ascii.digit_to_int 'A';
  assert (Char.Ascii.digit_of_int 5 = '5');
  assert (Char.Ascii.digit_of_int (-5) = '5');
  assert (Char.Ascii.digit_of_int 9 = '9');
  assert (Char.Ascii.digit_of_int (-9) = '9');
  assert (Char.Ascii.digit_of_int 10 = '0');
  assert (Char.Ascii.digit_of_int (-10) = '0');
  assert (Char.Ascii.digit_of_int 112 = '2');
  assert (Char.Ascii.digit_of_int (-112) = '2');
  ()

let test_hexadecimal () =
  assert (Char.Ascii.is_hex_digit '0');
  assert (Char.Ascii.is_hex_digit 'A');
  assert (Char.Ascii.is_hex_digit 'a');
  assert (Char.Ascii.is_hex_digit 'F');
  assert (Char.Ascii.is_hex_digit 'f');
  assert (not (Char.Ascii.is_hex_digit 'G'));
  assert (not (Char.Ascii.is_hex_digit 'g'));
  assert (Char.Ascii.hex_digit_to_int '5' = 5);
  assert (Char.Ascii.hex_digit_to_int 'f' = 0xF);
  assert (Char.Ascii.hex_digit_to_int 'F' = 0xF);
  assert (Char.Ascii.hex_digit_to_int 'a' = 0xA);
  assert (Char.Ascii.hex_digit_to_int 'A' = 0xA);
  assert (Char.Ascii.hex_digit_to_int 'd' = 0xD);
  assert_raise_invalid_argument Char.Ascii.hex_digit_to_int 'g';
  assert_raise_invalid_argument Char.Ascii.hex_digit_to_int 'G';
  assert_raise_invalid_argument Char.Ascii.hex_digit_to_int 'z';
  assert (Char.Ascii.lower_hex_digit_of_int 5 = '5');
  assert (Char.Ascii.lower_hex_digit_of_int (-5) = '5');
  assert (Char.Ascii.lower_hex_digit_of_int 10 = 'a');
  assert (Char.Ascii.lower_hex_digit_of_int (-10) = 'a');
  assert (Char.Ascii.lower_hex_digit_of_int 15 = 'f');
  assert (Char.Ascii.lower_hex_digit_of_int (-15) = 'f');
  assert (Char.Ascii.lower_hex_digit_of_int 16 = '0');
  assert (Char.Ascii.lower_hex_digit_of_int (-16) = '0');
  assert (Char.Ascii.lower_hex_digit_of_int (255) = 'f');
  assert (Char.Ascii.lower_hex_digit_of_int (-255) = 'f');
  assert (Char.Ascii.lower_hex_digit_of_int 0x77 = '7');
  assert (Char.Ascii.lower_hex_digit_of_int 0x7A = 'a');
  assert (Char.Ascii.lower_hex_digit_of_int 0xB = 'b');
  assert (Char.Ascii.lower_hex_digit_of_int 0x2 = '2');
  assert (Char.Ascii.upper_hex_digit_of_int 5 = '5');
  assert (Char.Ascii.upper_hex_digit_of_int (-5) = '5');
  assert (Char.Ascii.upper_hex_digit_of_int 10 = 'A');
  assert (Char.Ascii.upper_hex_digit_of_int (-10) = 'A');
  assert (Char.Ascii.upper_hex_digit_of_int 15 = 'F');
  assert (Char.Ascii.upper_hex_digit_of_int (-15) = 'F');
  assert (Char.Ascii.upper_hex_digit_of_int 16 = '0');
  assert (Char.Ascii.upper_hex_digit_of_int (-16) = '0');
  assert (Char.Ascii.upper_hex_digit_of_int (255) = 'F');
  assert (Char.Ascii.upper_hex_digit_of_int (-255) = 'F');
  assert (Char.Ascii.upper_hex_digit_of_int 0x77 = '7');
  assert (Char.Ascii.upper_hex_digit_of_int 0x7A = 'A');
  assert (Char.Ascii.upper_hex_digit_of_int 0xB = 'B');
  assert (Char.Ascii.upper_hex_digit_of_int 0x2 = '2');
  ()

let test_case_mappings () =
  assert (Char.Ascii.uppercase 'a' = 'A');
  assert (Char.Ascii.uppercase 'g' = 'G');
  assert (Char.Ascii.uppercase 'Z' = 'Z');
  assert (Char.Ascii.uppercase '\n' = '\n');
  assert (Char.Ascii.lowercase 'A' = 'a');
  assert (Char.Ascii.lowercase 'G' = 'g');
  assert (Char.Ascii.lowercase 'z' = 'z');
  assert (Char.Ascii.lowercase '\n' = '\n');
  ()


let tests () =
  test_constants ();
  test_is_valid ();
  test_preds ();
  test_decimal ();
  test_hexadecimal ();
  test_case_mappings();
  ()

let () =
  tests ();
  print_endline "OK"
