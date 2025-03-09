(* TEST
 modules = "minor_named_.c";
*)

(* Tests Callback.register and caml_named_value on a young object *)

external incr_ref : unit -> unit = "incr_ref"

let () =
  let r = ref 40 in
  Callback.register "incr_ref" r;
  incr_ref ();
  Gc.minor ();
  incr_ref ();
  Printf.printf "%d\n" !r
