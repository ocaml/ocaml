(* TEST
*)

let [@inline never] foo () =
  let s = "Hello" ^ " world!" in
  Gc.finalise_last (fun () -> print_endline "finalised") s;
  Gc.minor ();
  s

let [@inline never] bar () =
  let s = foo () in
  print_endline s

let _ =
  bar ();
  Gc.full_major ()
