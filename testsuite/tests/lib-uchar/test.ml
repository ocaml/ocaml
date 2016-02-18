(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                            Daniel C. Buenzli                           *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


let assert_raise_invalid_argument f v =
  assert (try ignore (f v); false with Invalid_argument _ -> true)

let test_constants () =
  assert (Uchar.(to_int min) = 0x0000);
  assert (Uchar.(to_int max) = 0x10FFFF);
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

let test_dump () =
  let str u = Format.asprintf "%a" Uchar.dump u in
  assert (str Uchar.min = "U+0000");
  assert (str Uchar.(succ min) = "U+0001");
  assert (str Uchar.(of_int 0xFFFF) = "U+FFFF");
  assert (str Uchar.(succ (of_int 0xFFFF)) = "U+10000");
  assert (str Uchar.(pred max) = "U+10FFFE");
  assert (str Uchar.max = "U+10FFFF");
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
  test_dump ();
  ()

let () =
  tests ();
  print_endline "OK"
