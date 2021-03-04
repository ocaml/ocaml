(* TEST
   include dynlink
*)

(* Make sure dynlink state info is accurate before any load
   occurs #9338. *)

let test () =
  assert (List.mem "Dynlink" (Dynlink.main_program_units ()));
  assert (List.mem "Dynlink" (Dynlink.all_units ()));
  ()

let () = test (); print_endline "OK"
