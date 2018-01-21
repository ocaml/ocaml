(* TEST
   flags = " -w A -strict-sequence "
   * toplevel
*)

(* Ignore OCAMLRUNPARAM=b to be reproducible *)
Printexc.record_backtrace false;;

let () = raise Exit; () ;; (* warn *)
