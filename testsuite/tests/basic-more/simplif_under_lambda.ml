(* TEST *)

let f () =
  let junk = ref 42 in
  Gc.finalise_last (fun () -> print_endline "collected") junk;
  let tuple = Sys.opaque_identity (junk, "ok") in
  match tuple with
  | _, ok ->
     let rec loop () =
       print_endline ok;
       if false then loop ()
     in
     loop

let () =
  let fn = f () in
  Gc.full_major ();
  fn ()
