(* TEST
   include systhreads;
   hassysthreads;
*)

(* This test creates [nb_keys] DLS keys, each storing an atomic integer.
   For each key, we spawn [nb_threads_per_key] threads to increment
   this integer concurrently.

   At the end of the computation, the sum of the value of the keys should be
   [nb_keys * nb_threads_per_key].

   But: we tweaked the [init] function passed to [Domain.DLS.new_key]
   to deschedule the current thread while it is running, which could
   result in thread-safety issues. In particular, the same key could
   be (re)initialized to 0 several times, returning in some increments
   being forgotten. The present test verifies that the [DLS.get]
   implementation avoids this issue.

   See #12889 for discussions of past thread-safety issues in DLS;
   there previously were races in both the array-resizing and the
   key-initialization logic that broke the present test.
*)

let pause () =
  Thread.yield ()

let init () =
  pause ();
  Atomic.make 0

let nb_keys = 10
let nb_threads_per_key = 3

let keys = Array.init nb_keys (fun _ -> Domain.DLS.new_key init)

let threads =
  keys
  |> Array.map (fun k ->
    Array.init nb_threads_per_key (fun _ ->
      Thread.create (fun () ->
        pause ();
        Atomic.incr (Domain.DLS.get k)
      ) ()
    )
  )

let () =
  Array.iter (Array.iter Thread.join) threads;
  let total =
    Array.fold_left (fun sum k ->
      sum + Atomic.get (Domain.DLS.get k)
    ) 0 keys
  in
  print_int total; print_newline ();
  assert (total = nb_keys * nb_threads_per_key)
