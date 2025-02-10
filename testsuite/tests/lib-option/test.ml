(* TEST *)

let strf = Printf.sprintf
let assert_raise_invalid_argument f v =
  assert (try ignore (f v); false with Invalid_argument _ -> true);
  ()

let test_none_some () =
  assert (Option.none = None);
  assert (Option.some 2 = Some 2);
  ()

let test_value () =
  let o = Some 3 in
  assert (Option.value ~default:5 o = 3);

  let o = None in
  assert (Option.value ~default:5 o = 5);
  ()

let test_get () =
  let o = Some 3 in
  assert (Option.get o = 3);

  let o = None in
  assert_raise_invalid_argument Option.get o;
  ()

let test_bind () =
  let o = Some 3 in
  assert (Option.bind o (fun x -> Some (succ x)) = Some 4);
  assert (Option.bind o (fun _ -> None) = None);

  let o = None in
  assert (Option.bind o (fun x -> Some (succ x)) = None);
  assert (Option.bind o (fun _ -> None) = None);
  ()

let test_join () =
  let oo = Some (Some 3) in
  assert (Option.join oo = Some 3);

  let oo = Some None in
  assert (Option.join oo = None);

  let oo = None in
  assert (Option.join oo = None);
  ()

let test_map () =
  let o = Some 3 in
  assert (Option.map succ o = Some 4);

  let o = None in
  assert (Option.map succ o = None);
  ()

let test_fold () =
  let o = Some 3 in
  assert (Option.fold ~none:0 ~some:succ o = 4);
  assert (Option.(fold ~none ~some) o = (Some 3));

  let o = None in
  assert (Option.fold ~none:0 ~some:succ o = 0);
  assert (Option.(fold ~none ~some) o = None);
  ()

let test_iter () =
  let count = ref 0 in
  let set_count x = count := x in

  let o = Some 3 in
  Option.iter set_count o;
  assert (!count = 3);

  let o = None in
  Option.iter set_count o;
  assert (!count = 3);
  ()

let test_is_none () =
  let o = Some 3 in
  assert (Option.is_none o = false);

  let o = None in
  assert (Option.is_none o = true);
  ()

let test_is_some () =
  let o = Some 3 in
  assert (Option.is_some o = true);

  let o = None in
  assert (Option.is_some o = false);
  ()

let test_equal () =
  let eq v0 v1 = (v0 mod 2) = (v1 mod 2) in
  let equal = Option.equal eq in
  assert (not @@ equal (Some 2) (Some 3));
  assert (       equal (Some 2) (Some 4));
  assert (not @@ equal (Some 2) None);
  assert (not @@ equal None (Some 3));
  assert (not @@ equal None (Some 4));
  assert (       equal None None);
  ()

let test_compare () =
  let compare v0 v1 = - (compare v0 v1) in
  let compare = Option.compare compare in
  assert (compare (Some 2) (Some 1) = -1);
  assert (compare (Some 2) (Some 2) = 0);
  assert (compare (Some 2) (Some 3) = 1);
  assert (compare (Some 2) None = 1);
  assert (compare None (Some 1) = -1);
  assert (compare None (Some 2) = -1);
  assert (compare None (Some 3) = -1);
  assert (compare None None = 0);
  ()

let test_to_option_list_seq () =
  assert (Option.to_result ~none:6 (Some 3) = Ok 3);
  assert (Option.to_result ~none:6 None = Error 6);
  assert (Option.to_list (Some 3) = [3]);
  assert (Option.to_list None = []);
  begin match (Option.to_seq (Some 3)) () with
  | Seq.Cons (3, f) -> assert (f () = Seq.Nil)
  | _ -> assert false
  end;
  assert ((Option.to_seq None) () = Seq.Nil);
  ()

let tests () =
  test_none_some ();
  test_value ();
  test_get ();
  test_bind ();
  test_join ();
  test_map ();
  test_fold ();
  test_iter ();
  test_is_none ();
  test_is_some ();
  test_equal ();
  test_compare ();
  test_to_option_list_seq ();
  ()

let () =
  tests ();
  print_endline "OK"
