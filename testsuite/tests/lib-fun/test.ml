(* TEST *)

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

let test_on () =
  (* usage: *)
  assert (Fun.on Int.abs Int.add (-3) 2 = 5);
  assert (Fun.on fst Int.compare (1, 2) (1, 3) = 0);
  assert (List.equal (Fun.on String.length Int.equal) ["hello"] ["world"]);
  (* properties: *)
  assert ( (* forall op, on id op = op *)
    Fun.on Fun.id Int.add (-1) 1 = Int.add (-1) 1
  );
  assert ( (* forall op1 op2, compose (on op1) (on op2) = on (compose op2 op1) *)
    let (%) f g x = f (g x) in
    (Fun.on String.to_seq % Fun.on String.of_seq) String.equal "nothing" "alike"
    =
    Fun.on (String.of_seq % String.to_seq) String.equal "nothing" "alike"
  );
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
  test_on ();
  test_negate ();
  test_protect ();
  ()

let () =
  tests ();
  print_endline "OK";
  ()
