(* TEST
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)


let _ =
  (* start a dummy domain and shut it down to cause a domain reuse *)
  let d = Domain.spawn (fun _ -> ()) in
  Domain.join d;
  let finished = Atomic.make false in
  let d = Domain.spawn (fun _ ->
    Unix.sleep 1;
    if not (Atomic.get finished) then
      print_endline "Should not reach here!") in
  Gc.full_major ();
  print_endline "OK";
  Atomic.set finished true;
  Domain.join d
