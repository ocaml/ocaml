(* TEST
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* Tests statmemprof behaviour when a callback terminates its thread.
   The expected behaviour is that the thread exits, but sampling
   continues in other threads of the same domain. Note that this test
   doesn't currently test that sampling continues!  *)

let _ =
  let main_thread = Thread.id (Thread.self ()) in
  let _:Gc.Memprof.t = Gc.Memprof.(start ~callstack_size:10 ~sampling_rate:1.
                { null_tracker with alloc_minor = fun _ ->
                      if Thread.id (Thread.self ()) <> main_thread then
                      raise Thread.Exit;
                      None })
  in
  let t = Thread.create (fun () ->
      ignore (Sys.opaque_identity (ref 1));
      assert false) ()
  in
  Thread.join t;
  Gc.Memprof.stop ()
