(* TEST *)

(* Domain 0 registers [Gc.finalise_last] on young values of another
   domain and keeps them alive. The other domain allocates all the time,
   thus it reuses its minor heap soon after each minor GC. No finaliser
   must run. *)

let () =
  let size = 16 in
  let count = Atomic.make 0 in
  let slot = Atomic.make None and stop = Atomic.make false in
  let d =
    Domain.spawn (fun () ->
      while not (Atomic.get stop) do
        (match Atomic.get slot with
         | None -> Atomic.set slot (Some (Array.init size (fun _ -> ref 42)))
         | Some _ -> ());
        ignore (Sys.opaque_identity (Array.make 8 0));
        Domain.cpu_relax ()
      done)
  in
  (* On one CPU, each minor GC waits until the other domain runs. The
     limit on the CPU time keeps the test short in that case. *)
  let keep = ref [] and gcs = ref 0 in
  while !gcs < 20000 && Sys.time () < 2.0 do
    (match Atomic.exchange slot None with
     | Some a ->
       Array.iter (Gc.finalise_last (fun () -> Atomic.incr count)) a;
       keep := a :: !keep
     | None -> ());
    Gc.minor ();
    incr gcs
  done;
  Gc.full_major ();
  Atomic.set stop true;
  Domain.join d;
  assert (Atomic.get count = 0);
  List.iter (Array.iter (fun r -> assert (!r = 42))) !keep;
  print_endline "ok"
