(* TEST *)

let expect_failure ~exn f =
  assert (match f () with _ -> false | exception exn' -> exn = exn')

let test_consts () =
  assert (Int16.(zero = of_int 0));
  assert (Int16.(one = of_int 1));
  assert (Int16.(minus_one = of_int (-1)));
  assert (Int16.(min_int = of_int (-32768)));
  assert (Int16.(max_int = of_int 32767));

  ()

let test_arith () =
  (* Basic arithmetic. *)
  assert (Int16.(add (of_int 2) (of_int 4) = of_int 6));
  assert (Int16.(sub (of_int 6) (of_int 2) = of_int 4));
  assert (Int16.(mul (of_int 6) (of_int 2) = of_int 12));
  assert (Int16.(div (of_int 12) (of_int 2) = of_int 6));
  assert (Int16.(rem (of_int 5) (of_int 2) = of_int 1));
  assert (Int16.(succ (of_int 5) = of_int 6));
  assert (Int16.(pred (of_int 5) = of_int 4));
  assert (Int16.(abs (of_int (-5)) = of_int 5));
  assert (Int16.(abs (of_int 5) = of_int 5));

  (* Overflow behavior. *)
  assert (Int16.(add max_int one = min_int));
  assert (Int16.(sub min_int one = max_int));
  assert (Int16.(mul (of_int 16384) (of_int 2) = of_int (-32768)));
  assert (Int16.(neg min_int = min_int));
  assert (Int16.(abs min_int = min_int));

  (* Unsigned arithmetic. *)
  assert (Int16.(unsigned_div (of_int (-1)) (of_int 2) = of_int 32767));
  assert (Int16.(unsigned_rem (of_int (-1)) (of_int 2) = of_int 1));

  (* Division by zero behavior. *)
  expect_failure ~exn:Division_by_zero (fun () ->
      Int16.(div (of_int 10) (of_int 0)));
  expect_failure ~exn:Division_by_zero (fun () ->
      Int16.(unsigned_div (of_int 10) (of_int 0)));
  expect_failure ~exn:Division_by_zero (fun () ->
      Int16.(rem (of_int 10) (of_int 0)));
  expect_failure ~exn:Division_by_zero (fun () ->
      Int16.(unsigned_rem (of_int 10) (of_int 0)));

  ()

let test_logops () =
  assert (Int16.(logand (of_int 0xF000) (of_int 0xFFFF) = of_int 0xF000));
  assert (Int16.(logor (of_int 0x0FFF) (of_int 0xF000) = of_int 0xFFFF));
  assert (Int16.(logxor (of_int 0xFFFF) (of_int 0x0FFF) = of_int 0xF000));
  assert (Int16.(lognot max_int = min_int));
  assert (Int16.(shift_left (of_int 1) 12 = of_int 4096));
  assert (Int16.(shift_left (of_int 0x1000) 4 = of_int 0));

  (* Overflow behavior. *)
  assert (Int16.(shift_right (of_int 4096) 12 = of_int 1));
  assert (Int16.(shift_right (of_int (-4096)) 12 = of_int (-1)));
  assert (Int16.(shift_right_logical min_int 15 = of_int 1));

  ()

let test_conversions () =
  (* Signed conversions. *)
  assert (Int16.(of_int 32767 = of_int 32767));
  assert (Int16.(of_int 32768 = of_int (-32768)));
  assert (Int16.(of_int 98303 = of_int 32767));
  assert (Int16.(of_int (-32769) = of_int 32767));
  assert (Int16.(of_int (-65535) = of_int 1));
  assert (Int16.(to_int (of_int 32767)) = 32767);
  assert (Int16.(to_int (of_int (-32768))) = -32768);

  (* Unsigned conversions. *)
  assert (Int16.(unsigned_to_int (of_int 32767)) = Some 32767);
  assert (Int16.(unsigned_to_int (of_int (-1))) = Some 65535);
  assert (Int16.(unsigned_to_int (of_int (-32768))) = Some 32768);

  (* String conversions. *)
  let assert_string s n =
    [ s; String.uppercase_ascii s ]
    |> List.iter (fun s -> assert (Int16.(of_string s = of_int n)))
  in
  assert_string "32767" 32767;
  assert_string "3_2_7_6_7" 32767;
  assert_string "0u65535" (-1);
  assert_string "-32768" (-32768);
  assert_string "0x7fff" 32767;
  assert_string "-0x8000" (-32768);
  assert_string "0b0111111111111111" 32767;
  assert_string "-0b1000000000000000" (-32768);
  assert_string "0o77777" 32767;
  assert_string "-0o100000" (-32768);

  (* Parsing failures. *)
  expect_failure ~exn:(Failure "Int16.of_string") (fun () ->
      Int16.of_string "32768");
  expect_failure ~exn:(Failure "Int16.of_string") (fun () ->
      Int16.of_string "-32769");
  expect_failure ~exn:(Failure "Int16.of_string") (fun () ->
      Int16.of_string "~abc~");

  assert (Int16.(to_string (of_int 32767)) = "32767");
  assert (Int16.(to_string (of_int (-32768))) = "-32768");
  assert (Int16.(to_string (of_int (-1))) = "-1");

  ()


let test_compare () =
  (* Signed comparison. *)
  assert (Int16.(compare (of_int 3) (of_int 3) = 0));
  assert (Int16.(compare (of_int 3) (of_int 4) = -1));
  assert (Int16.(compare (of_int 4) (of_int 3) = 1));
  assert (Int16.(compare (of_int (-4)) (of_int 3) = -1));
  assert (Int16.(compare (of_int 3) (of_int (-4)) = 1));

  (* Unsigned comparison. *)
  assert (Int16.(unsigned_compare (of_int 3) (of_int 3) = 0));
  assert (Int16.(unsigned_compare (of_int 3) (of_int 4) = -1));
  assert (Int16.(unsigned_compare (of_int 4) (of_int 3) = 1));
  assert (Int16.(unsigned_compare (of_int (-1)) (of_int 1) = 1));
  assert (Int16.(unsigned_compare (of_int 1) (of_int (-1)) = -1));

  (* Equality. *)
  assert (Int16.(equal (of_int 1) (of_int 1)) = true);
  assert (Int16.(equal (of_int 1) (of_int 0)) = false);

  (* Minimum/maximum. *)
  assert (Int16.(max (of_int 2) (of_int 3) = of_int 3));
  assert (Int16.(min (of_int 2) (of_int 3) = of_int 2));
  assert (Int16.(max min_int max_int = max_int));
  assert (Int16.(min min_int max_int = min_int));

  ()

let tests () =
  test_consts ();
  test_arith ();
  test_logops ();
  test_conversions ();
  test_compare ();
  ()

let () =
  tests ();
  print_endline "OK"
