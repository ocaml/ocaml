(* TEST *)

let expect_failure ~exn f =
  assert (match f () with _ -> false | exception exn' -> exn = exn')

let test_consts () =
  assert (Int8.(zero = of_int 0));
  assert (Int8.(one = of_int 1));
  assert (Int8.(minus_one = of_int (-1)));
  assert (Int8.(min_int = of_int (-128)));
  assert (Int8.(max_int = of_int 127));

  ()

let test_arith () =
  (* Basic arithmetic. *)
  assert (Int8.(add (of_int 2) (of_int 4) = of_int 6));
  assert (Int8.(sub (of_int 6) (of_int 2) = of_int 4));
  assert (Int8.(mul (of_int 6) (of_int 2) = of_int 12));
  assert (Int8.(div (of_int 12) (of_int 2) = of_int 6));
  assert (Int8.(rem (of_int 5) (of_int 2) = of_int 1));
  assert (Int8.(succ (of_int 5) = of_int 6));
  assert (Int8.(pred (of_int 5) = of_int 4));
  assert (Int8.(abs (of_int (-5)) = of_int 5));
  assert (Int8.(abs (of_int 5) = of_int 5));

  (* Overflow behavior. *)
  assert (Int8.(add max_int one = min_int));
  assert (Int8.(sub min_int one = max_int));
  assert (Int8.(mul (of_int 64) (of_int 2) = of_int (-128)));
  assert (Int8.(neg min_int = min_int));
  assert (Int8.(abs min_int = min_int));

  (* Unsigned arithmetic. *)
  assert (Int8.(unsigned_div (of_int (-1)) (of_int 2) = of_int 127));
  assert (Int8.(unsigned_rem (of_int (-1)) (of_int 2) = of_int 1));

  (* Division by zero behavior. *)
  expect_failure ~exn:Division_by_zero (fun () ->
      Int8.(div (of_int 10) (of_int 0)));
  expect_failure ~exn:Division_by_zero (fun () ->
      Int8.(unsigned_div (of_int 10) (of_int 0)));
  expect_failure ~exn:Division_by_zero (fun () ->
      Int8.(rem (of_int 10) (of_int 0)));
  expect_failure ~exn:Division_by_zero (fun () ->
      Int8.(unsigned_rem (of_int 10) (of_int 0)));

  ()

let test_logops () =
  assert (Int8.(logand (of_int 0xF0) (of_int 0xFF) = of_int 0xF0));
  assert (Int8.(logor (of_int 0x0F) (of_int 0xF0) = of_int 0xFF));
  assert (Int8.(logxor (of_int 0xFF) (of_int 0x0F) = of_int 0xF0));
  assert (Int8.(lognot max_int = min_int));
  assert (Int8.(shift_left (of_int 1) 4 = of_int 16));
  assert (Int8.(shift_left (of_int 0x10) 4 = of_int 0));

  (* Overflow behavior. *)
  assert (Int8.(shift_right (of_int 16) 4 = of_int 1));
  assert (Int8.(shift_right (of_int (-16)) 4 = of_int (-1)));
  assert (Int8.(shift_right_logical min_int 7 = of_int 1));

  ()

let test_conversions () =
  (* Signed conversions. *)
  assert (Int8.(of_int 127 = of_int 127));
  assert (Int8.(of_int 128 = of_int (-128)));
  assert (Int8.(of_int 383 = of_int 127));
  assert (Int8.(of_int (-129) = of_int 127));
  assert (Int8.(of_int (-255) = of_int 1));
  assert (Int8.(to_int (of_int 127)) = 127);
  assert (Int8.(to_int (of_int (-128))) = -128);

  (* Unsigned conversions. *)
  assert (Int8.(unsigned_to_int (of_int 127)) = Some 127);
  assert (Int8.(unsigned_to_int (of_int (-1))) = Some 255);
  assert (Int8.(unsigned_to_int (of_int (-128))) = Some 128);

  (* String conversions. *)
  let assert_string s n =
    [ s; String.uppercase_ascii s ]
    |> List.iter (fun s -> assert (Int8.(of_string s = of_int n)))
  in
  assert_string "127" 127;
  assert_string "1_2_7" 127;
  assert_string "0u255" (-1);
  assert_string "-128" (-128);
  assert_string "0x7f" 127;
  assert_string "-0x80" (-128);
  assert_string "0b01111111" 127;
  assert_string "-0b10000000" (-128);
  assert_string "0o177" 127;
  assert_string "-0o200" (-128);

  (* Parsing failures. *)
  expect_failure ~exn:(Failure "Int8.of_string") (fun () ->
      Int8.of_string "128");
  expect_failure ~exn:(Failure "Int8.of_string") (fun () ->
      Int8.of_string "-129");
  expect_failure ~exn:(Failure "Int8.of_string") (fun () ->
      Int8.of_string "~abc~");

  assert (Int8.(to_string (of_int 127)) = "127");
  assert (Int8.(to_string (of_int (-128))) = "-128");
  assert (Int8.(to_string (of_int (-1))) = "-1");

  ()

let test_compare () =
  (* Signed comparison. *)
  assert (Int8.(compare (of_int 3) (of_int 3) = 0));
  assert (Int8.(compare (of_int 3) (of_int 4) = -1));
  assert (Int8.(compare (of_int 4) (of_int 3) = 1));
  assert (Int8.(compare (of_int (-4)) (of_int 3) = -1));
  assert (Int8.(compare (of_int 3) (of_int (-4)) = 1));

  (* Unsigned comparison. *)
  assert (Int8.(unsigned_compare (of_int 3) (of_int 3) = 0));
  assert (Int8.(unsigned_compare (of_int 3) (of_int 4) = -1));
  assert (Int8.(unsigned_compare (of_int 4) (of_int 3) = 1));
  assert (Int8.(unsigned_compare (of_int (-1)) (of_int 1) = 1));
  assert (Int8.(unsigned_compare (of_int 1) (of_int (-1)) = -1));

  (* Equality. *)
  assert (Int8.(equal (of_int 1) (of_int 1)) = true);
  assert (Int8.(equal (of_int 1) (of_int 0)) = false);

  (* Minimum/maximum. *)
  assert (Int8.(max (of_int 2) (of_int 3) = of_int 3));
  assert (Int8.(min (of_int 2) (of_int 3) = of_int 2));
  assert (Int8.(max min_int max_int = max_int));
  assert (Int8.(min min_int max_int = min_int));

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
