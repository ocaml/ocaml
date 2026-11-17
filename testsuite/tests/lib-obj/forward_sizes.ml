(* TEST *)

(* Obj.new_block can make Forward_tag blocks of any size.  The minor GC
   only short-circuits one-word Forward blocks; others must be copied
   like ordinary blocks (GitHub #14571). *)

let make_forward n =
  let b = Obj.new_block Obj.forward_tag n in
  for i = 0 to n - 1 do
    Obj.set_field b i (Obj.repr (string_of_int (100 + i)))
  done;
  b

let check b n =
  assert (Obj.tag b = Obj.forward_tag);
  assert (Obj.size b = n);
  for i = 0 to n - 1 do
    assert (Obj.obj (Obj.field b i) = string_of_int (100 + i))
  done

let () =
  let b2 = make_forward 2 in
  let b3 = make_forward 3 in
  Gc.minor ();          (* promote them *)
  check b2 2;
  check b3 3;
  Gc.full_major ();
  check b2 2;
  check b3 3;
  print_endline "OK"
