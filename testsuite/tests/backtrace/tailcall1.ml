let g () = raise Not_found [@@inline]

let h () =
  (* "g ()" is inlined by native compiler, but the location of the call should
     be dropped from the backtrace as it is in tail position. *)
  g ()
[@@inline never]

let () =
  Printexc.record_backtrace true;
  h ()
