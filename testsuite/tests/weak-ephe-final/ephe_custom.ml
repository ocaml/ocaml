(* TEST *)

let w = Weak.create 1

let major_obj () =
  let n = Sys.opaque_identity 42 in
  let v = Sys.opaque_identity (Int64.of_int n) in
  Gc.minor ();
  v

let () =
  Weak.set w 0 (Some (major_obj ()));
  Gc.major ();
  let x = Option.get (Weak.get_copy w 0) in
  Gc.major ();
  Printf.printf "value: %Ld\n%!" x;
  let junk = List.init 1_000_000 Fun.id in
  Gc.minor ();
  ignore (Sys.opaque_identity junk);
  (* check that the memory representing x did not get reused in junk *)
  Printf.printf "value: %Ld\n%!" x
