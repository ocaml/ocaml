(* TEST *)

let test_make () =
  let () = assert (Pair.make 1 "x" = (1, "x")) in
  let () = assert (Pair.make 'e' [] = ('e', [])) in
  ()
let test_swap () =
  let () = assert (Pair.swap (1, "x") = ("x", 1)) in
  let () = assert (Pair.swap ('e', []) = ([], 'e')) in
  ()

let test_fold () =
  let () = assert (Pair.fold (+) (2, 3) = 5) in
  let is_same i s = (s = string_of_int i) in
  let () = assert (Pair.fold is_same (2, "2")) in
  ()

let test_map () =
  let () = assert (Pair.map succ (fun a -> a ^ a) (3, "a") = (4, "aa")) in
  ()

let test_iter () =
  let iterator = ref 0 in
  let add x = iterator := !iterator + x in
  let mul x = iterator := !iterator * x in
  let () = Pair.iter add mul (4, 3) in
  let () = assert (!iterator = 12) in
  ()

let test_map2 () =
  let () = assert (Pair.map2 (+) (^) (3, "a") (2, "x") = (5, "ax")) in
  ()

let test_iter2 () =
  let iterator = ref 0 in
  let add_diff x y = iterator := !iterator + (y - x) in
  let mul_diff x y = iterator := !iterator * (y - x) in
  let () = Pair.iter2 add_diff mul_diff (1, 1) (3, 4) in
  let () = assert (!iterator = 6) in
  ()

let test_map_fst () =
  let () = assert (Pair.map_fst succ (1, "s") = (2, "s")) in
  let () = assert (Pair.map_fst (Fun.const []) (None, 'e') = ([], 'e')) in
  ()

let test_map_snd () =
  let () = assert (Pair.map_snd succ ("s", 1) = ("s", 2)) in
  let () = assert (Pair.map_snd (Fun.const []) ('e', None) = ('e', [])) in
  ()

let test_iter_fst () =
  let iterator = ref 0 in
  let add x = iterator := !iterator + x in
  let () = Pair.iter_fst add (3, 5) in
  let () = assert (!iterator = 3) in
  ()

let test_iter_snd () =
  let iterator = ref 0 in
  let add x = iterator := !iterator + x in
  let () = Pair.iter_snd add (3, 5) in
  let () = assert (!iterator = 5) in
  ()

let test_equal () =
  assert (Pair.equal (fun _ _ -> true) String.equal (3, "e") (4, "e")) ;
  assert (Pair.equal Int.equal String.equal (3, "e") (4, "e") = false)

let test_compare () =
  (* reflexive *)
  assert (Pair.compare Int.compare String.compare (3, "a") (3, "a") = 0) ;
  (* symmetric (and total) *)
  assert (Pair.compare Int.compare Char.compare (3, '3') (4, '4')
         * Pair.compare Int.compare Char.compare (4, '4') (3, '3') < 0) ;
  (* transitive *)
  let cmp1 = Pair.compare Int.compare Char.compare (3, '3') (4, '4') in
  let cmp2 = Pair.compare Int.compare Char.compare (4, '4') (5, '5') in
  let cmp3 = Pair.compare Int.compare Char.compare (3, '3') (5, '5') in
  if cmp1 * cmp2 > 0 then (* 3 < 4 < 5 or 3 > 4 > 5 *)
    assert (cmp1 * cmp3 > 0)

let tests () =
  test_make ();
  test_swap ();
  test_fold ();
  test_map ();
  test_iter ();
  test_map2 ();
  test_iter2 ();
  test_map_fst ();
  test_map_snd ();
  test_iter_fst ();
  test_iter_snd ();
  test_equal ();
  test_compare ();
  ()

let () =
  tests ();
  print_endline "OK"
