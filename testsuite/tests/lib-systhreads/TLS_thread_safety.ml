(* TEST
   include systhreads;
   hassysthreads;
*)

(* This is a TLS counterpart of
     tests/lib-domain/DLS_thread_safety.ml
   You should go there for explanations on what this test is checking.
*)

let pause () =
  Thread.yield ()

module TLS = Thread_local_storage

let init () =
  pause ();
  ref 0

let nb_keys = 10
let nb_threads_per_key = 3

let keys = Array.init nb_keys (fun _ -> TLS.make init)

let threads =
  keys
  |> Array.map (fun k ->
    Array.init nb_threads_per_key (fun _ ->
      Thread.create (fun () ->
        pause ();
        incr (TLS.get k)
      ) ()
    )
  )

let () =
  Array.iter (Array.iter Thread.join) threads;
  let total =
    Array.fold_left (fun sum k ->
      sum + !(TLS.get k)
    ) 0 keys
  in
  print_int total; print_newline ();
  assert (total = nb_keys * nb_threads_per_key)
