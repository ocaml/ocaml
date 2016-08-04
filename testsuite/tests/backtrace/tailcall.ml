let g () = raise Not_found

let h () =
  (* "g ()" is inlined by native compiler, but the location of the call should
     be dropped from the backtrace as it is in tail position. *)
  g ()

let () =
  Printexc.record_backtrace true;
  h ()
