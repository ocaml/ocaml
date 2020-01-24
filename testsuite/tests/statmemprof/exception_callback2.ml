(* TEST
*)

(* Check that Memprof callbacks cannot raise exceptions like Failure
   or Not_found out of thin air. *)

open Gc.Memprof

(* Makes sure that the Memprof queue is purged and does not cause
   uncaught exceptions later on. *)
let rec really_do_stop_memprof () =
  try stop () with _ -> really_do_stop_memprof ()

let pass () = really_do_stop_memprof () ; exit 0

let fail () = really_do_stop_memprof () ; exit 1

let _ =
  try
    let tracker = { null_tracker with
                    alloc_minor = (fun _ -> failwith "");
                    alloc_major = (fun _ -> failwith "");
                  }
    in
    start ~callstack_size:10 ~sampling_rate:1. tracker ;
    ignore (Sys.opaque_identity (Array.make 200 0)) ;
    stop ()
  with
  | Failure _ -> fail ()
  | _ -> pass ()

let _ = fail () (* the test was not performed *)
