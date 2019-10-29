
(*
  - create a record with a mutable field that has a lazy value in it
  - force a minor_gc to make sure that record is on the heap
  - update the lazy value to be a minor heap value
  - force the lazy value to be a forward to an item in the minor heap
  - call minor_gc and watch it fail the assert which makes sure that all remembered set items have been forwarded
*)

type test_record = {
  mutable lzy: string Lazy.t;
}

let is_shared x = Obj.is_shared (Obj.repr x)

let glbl_string = ref "init"

let get_random_string () =
  Printf.sprintf "%f" (Random.float 1.)

let get_lazy () =
  lazy (glbl_string := get_random_string (); !glbl_string)

let get_lazy_status x =
  if Lazy.is_val x then
    Printf.sprintf "%s" (Lazy.force x)
  else
    "<not forced>"

let dump_record_status x =
  Printf.printf "x.lzy=%s [shared=%b]\n" (get_lazy_status x.lzy) (is_shared x.lzy)

let update_record x =
  let lzy = get_lazy () in
  Printf.printf "updating: %b\n%!" (is_shared lzy);
  x.lzy <- lzy;
  dump_record_status x

let force_lazy_val x =
  let v = Lazy.force x.lzy in
  Printf.printf "forcing x.lzy [%s] %b %d\n%!" v (is_shared x.lzy) (Obj.tag (Obj.repr x.lzy))

let do_minor_gc () =
  Printf.printf "Gc.minor ()\n%!";
  Gc.minor ()

let () =
  Random.init 34;
  let lzy1 = get_lazy () in
  let x = {lzy=lzy1} in

  do_minor_gc ();
  (* x should now be on the heap *)
  dump_record_status x;
  Printf.printf "x is setup on major heap\n\n%!";

  update_record x;
  force_lazy_val x;
  dump_record_status x;
  do_minor_gc ();
  dump_record_status x
