(* TEST
*)

let errormsg f s =
  try
    let _ = f s in
    None
  with
  Failure msg -> Some msg

let int_of_string_errormsg s = errormsg int_of_string s

let int_of_string_32_errormsg s = errormsg Int32.of_string s

let int_of_string_64_errormsg s = errormsg Int64.of_string s

let nativeint_tests () =
  assert (int_of_string_errormsg "123" = None);
  assert (int_of_string_errormsg "a123" =
    Some "int_of_string: wrong char 'a' for base 10");
  assert (int_of_string_errormsg "0xy" =
    Some "int_of_string: wrong char 'y' for base 16");
  assert (int_of_string_errormsg "0x123fx" =
    Some "int_of_string: unconverted trailing characters: 'x' for base 16");
  assert (int_of_string_errormsg "0x123xf" =
    Some "int_of_string: unconverted trailing characters: 'xf' for base 16");
  assert (int_of_string_errormsg "999999999999999999999" = Some (
    "int_of_string: overflow while converting '999999999999999999999' to int"));
  ()

let int32_tests () =
  assert (int_of_string_32_errormsg "2147483648" = Some (
    "Int32.of_string: overflow: value '2147483648' " ^
    "won't fit into signed 32bit int"));
  (*
   * assert (int_of_string_32_errormsg "0x80000000" = Some (
   *   "Int32.of_string: overflow: value '0x80000000' " ^
   *   "won't fit into unsigned 32bit int"));
   * assert (int_of_string_32_errormsg "020000000000" = Some (
   *   "Int32.of_string: overflow: value '020000000000' " ^
   *   "won't fit into unsigned 32bit int"));
   * assert (int_of_string_32_errormsg "0b10000000000000000000000000000000" =
   *   Some ("Int32.of_string: overflow: value " ^
   *   "'0b10000000000000000000000000000000' won't fit into unsigned 32bit int"));
   *)
  ()

let int64_tests () =
  assert (int_of_string_64_errormsg "123" = None);
  assert (int_of_string_64_errormsg "a123" =
    Some "Int64.of_string: wrong char 'a' for base 10");
  assert (int_of_string_64_errormsg "0xy" =
    Some "Int64.of_string: wrong char 'y' for base 16");
  assert (int_of_string_64_errormsg "0x123fx" =
    Some "Int64.of_string: unconverted trailing characters: 'x' for base 16");
  assert (int_of_string_64_errormsg "0x123xf" =
    Some "Int64.of_string: unconverted trailing characters: 'xf' for base 16");
  assert (int_of_string_64_errormsg "999999999999999999999" = Some (
    "Int64.of_string: overflow while converting '999999999999999999999' " ^
    "to int"));
  assert (int_of_string_64_errormsg "9999999999999999999" = Some (
    "Int64.of_string: overflow: value '9999999999999999999' " ^
    "won't fit into signed 64bit int"));
  ()

let () =
  nativeint_tests ();
  int32_tests ();
  int64_tests ()
