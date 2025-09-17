(* TEST *)

let w = Weak.create 1

let major_obj () =
  let n = Sys.opaque_identity 42 in
  let v = Sys.opaque_identity (Int64.of_int n) in
  Gc.full_major ();
  v (* [v] is unmarked *)

let () =
  Weak.set w 0 (Some (major_obj ()));
  (* [v] is unmarked *)
  let x = Option.get (Weak.get_copy w 0) in
  (* [v] is marked by [Weak.get_copy] (which does not copy the custom object. *)
  Gc.major ();
  Printf.printf "value: %Ld\n%!" x;
  let junk = List.init 1_000_000 Fun.id in
  Gc.minor ();
  ignore (Sys.opaque_identity junk);
  (* check that the memory representing x did not get reused in junk *)
  Printf.printf "value: %Ld\n%!" x
