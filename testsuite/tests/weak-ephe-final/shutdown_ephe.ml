(* TEST
 ocamlrunparam += ",c=1";
 native;
*)

(* Last-domain termination with ephemeron data reachable only through
   ephemerons: orphaning force-alives the data, and the resulting
   (large) marking work must be drained before the mark stack is torn
   down. Runs with OCAMLRUNPARAM=c=1 to exercise caml_shutdown; the
   failure mode is a debug-runtime assertion at teardown. *)

let keys = Array.init 4 (fun i -> ref i)
let ephes = Array.init 4 (fun i ->
  let e = Obj.Ephemeron.create 1 in
  Obj.Ephemeron.set_key e 0 (Obj.repr keys.(i));
  Obj.Ephemeron.set_data e (Obj.repr (Array.init 300_000 (fun j -> ref j)));
  e)

let () =
  Gc.full_major ();
  ignore (Sys.opaque_identity (keys, ephes));
  print_endline "ok"
