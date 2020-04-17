(* TEST
*)

let test_id () =
  assert (Fun.id true = true);
  assert (Fun.id 1 = 1);
  assert (not (Fun.id nan = nan));
  ()

let test_const () =
  assert (Fun.const true false = true);
  assert (Fun.const 0 false = 0);
  assert (Fun.const 0 4 = 0);
  ()

let test_flip () =
  assert (Fun.flip ( ^ ) "of order" "out " = "out of order");
  assert (Fun.flip List.append [2] [1] = [1;2]);
  assert (Fun.flip List.cons [2] 1 = [1;2]);
  ()

let test_negate () =
  assert (Fun.negate (Bool.equal true) true = false);
  assert (Fun.negate (Bool.equal true) false = true);
  ()

let test_protect () =
  let does_raise f x =
    try f x ; false
    with _ -> true
  in
  let double_raise () =
    let f () = raise Exit in
    try
      Fun.protect ~finally:f f ()
    with
    | Exit -> ()
  in
  assert (does_raise double_raise ())

let tests () =
  test_id ();
  test_const ();
  test_flip ();
  test_negate ();
  test_protect ();
  ()

let () =
  tests ();
  print_endline "OK";
  ()
