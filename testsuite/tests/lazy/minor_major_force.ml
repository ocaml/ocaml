(* TEST
   ocamlopt_flags += " -O3 "
*)

(*
  - create a record with a mutable field that has a lazy value in it
  - force a minor_gc to make sure that record is on the heap
  - update the lazy value to be a minor heap value
  - force the lazy value to be a forward to an item in the minor heap
  - call minor_gc and watch it fail the assert which makes sure that all remembered set items have been forwarded
*)

type test_record = {
  mutable lzy_str: string Lazy.t;
  mutable lzy_int: int Lazy.t;
}


external is_shared : 'a -> bool = "caml_obj_is_shared"

let glbl_int = ref 0
let glbl_string = ref "init"

let get_random_int () =
  Random.int 256

let get_random_string () =
  Printf.sprintf "%f" (Random.float 1.)


let get_lazy_status fmt_str x =
  if Lazy.is_val x then
    Printf.sprintf fmt_str (Lazy.force x)
  else
    "<not forced>"

let get_lazy_int_status x = get_lazy_status "%d" x
let get_lazy_string_status x = get_lazy_status "%s" x

let dump_record_status x =
  Printf.printf "x.lzy_string=%s [shared=%b]\n" (get_lazy_string_status x.lzy_str) (is_shared x.lzy_str);
  Printf.printf "x.lzy_int=%s [shared=%b]\n" (get_lazy_int_status x.lzy_int) (is_shared x.lzy_int)

let force_lazy_vals x =
  let v = Lazy.force x.lzy_str in
  Printf.printf "forcing x.lzy_str [%s] %b %d\n%!" v (is_shared x.lzy_str) (Obj.tag (Obj.repr x.lzy_str));
  let v = Lazy.force x.lzy_int in
  Printf.printf "forcing x.lzy_int [%d] %b %d\n%!" v (is_shared x.lzy_int) (Obj.tag (Obj.repr x.lzy_int))

let do_minor_gc () =
  Printf.printf "Gc.minor ()\n%!";
  Gc.minor ()

let () =
  Random.init 34;
  let x = {
    lzy_str = lazy (glbl_string := get_random_string (); !glbl_string);
    lzy_int = lazy (glbl_int := get_random_int (); !glbl_int);
  } in

  do_minor_gc ();
  (* x should now be on the heap *)
  dump_record_status x;
  Printf.printf "x is setup on major heap\n\n%!";

  Printf.printf "updating fields in x\n\n%!";
  x.lzy_str <- lazy (glbl_string := get_random_string (); !glbl_string);
  x.lzy_int <- lazy (glbl_int := get_random_int (); !glbl_int);
  dump_record_status x;

  force_lazy_vals x;
  dump_record_status x;
  do_minor_gc ();
  dump_record_status x
