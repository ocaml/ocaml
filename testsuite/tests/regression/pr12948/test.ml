(* TEST
   include systhreads;
   hassysthreads;
   bytecode;
 *)

let () =
  let domain = Domain.spawn (fun () ->
      Thread.join @@ Thread.create (fun () -> ()) ();
      Unix.gettimeofday ()
    )
  in
  let t0 = Domain.join domain in
  let t1 = Unix.gettimeofday () in
  (* Printf.printf "%g %g %g\n" t0 t1 (t1 -. t0); *)
  assert (t1 -. t0 < 0.050)
