(* TEST
* hassysthreads
include systhreads
** bytecode
** native
*)

let _ =
  let main_thread = Thread.id (Thread.self ()) in
  Gc.Memprof.(start ~callstack_size:10 ~sampling_rate:1.
                { null_tracker with alloc_minor = fun _ ->
                      if Thread.id (Thread.self ()) <> main_thread then
                        Thread.exit ();
                      None });
  let t = Thread.create (fun () ->
      ignore (Sys.opaque_identity (ref 1));
      assert false) ()
  in
  Thread.join t;
  Gc.Memprof.stop ()

let _ =
  Gc.Memprof.(start ~callstack_size:10 ~sampling_rate:1.
    { null_tracker with alloc_minor = fun _ -> Thread.exit (); None });
  ignore (Sys.opaque_identity (ref 1));
  assert false
