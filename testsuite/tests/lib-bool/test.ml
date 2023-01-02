(* TEST
*)

let test_not () =
  assert (Bool.not false = true);
  assert (Bool.not true = false);
  ()

let test_and () =
  let wit = ref 0 in
  assert (Bool.( && ) (incr wit; false) (incr wit; false) = false);
  assert (!wit = 1); wit := 0;
  assert (Bool.( && ) (incr wit; false) (incr wit; true) = false);
  assert (!wit = 1); wit := 0;
  assert (Bool.( && ) (incr wit; true) (incr wit; false) = false);
  assert (!wit = 2); wit := 0;
  assert (Bool.( && ) (incr wit; true) (incr wit; true) = true);
  assert (!wit = 2); wit := 0;
  ()

let test_or () =
  let wit = ref 0 in
  assert (Bool.( || ) (incr wit; false) (incr wit; false) = false);
  assert (!wit = 2); wit := 0;
  assert (Bool.( || ) (incr wit; false) (incr wit; true) = true);
  assert (!wit = 2); wit := 0;
  assert (Bool.( || ) (incr wit; true) (incr wit; false) = true);
  assert (!wit = 1); wit := 0;
  assert (Bool.( || ) (incr wit; true) (incr wit; true) = true);
  assert (!wit = 1); wit := 0;
  ()

let test_equal () =
  assert (Bool.equal false false = true);
  assert (Bool.equal false true = false);
  assert (Bool.equal true false = false);
  assert (Bool.equal true true = true);
  ()

let test_compare () =
  assert (Bool.compare false false = 0);
  assert (Bool.compare false true = -1);
  assert (Bool.compare true false = 1);
  assert (Bool.compare true true = 0);
  ()

let test_to_int () =
  assert (Bool.to_int false = 0);
  assert (Bool.to_int true = 1);
  ()

let test_to_float () =
  assert (Bool.to_float false = 0.);
  assert (Bool.to_float true = 1.);
  ()

let test_of_string () =
  (*
  assert (Bool.of_string "false" = Some false);
  assert (Bool.of_string "true" = Some true);
  assert (Bool.of_string "heyho" = None);
  assert (Bool.of_string "1" = None);
  assert (Bool.of_string "0" = None);
*)
  ()

let test_to_string () =
  assert (Bool.to_string false = "false");
  assert (Bool.to_string true = "true");
  ()


let test_hash () =
  let f b =
    assert (Hashtbl.hash b = Bool.hash b);
    assert (Hashtbl.seeded_hash 16 b = Bool.seeded_hash 16 b)
  in
  f true; f false

let tests () =
  test_not ();
  test_and ();
  test_or ();
  test_equal ();
  test_compare ();
  test_to_int ();
  test_to_float ();
  test_of_string ();
  test_to_string ();
  test_hash ();
  ()

let () =
  tests ();
  print_endline "OK"
