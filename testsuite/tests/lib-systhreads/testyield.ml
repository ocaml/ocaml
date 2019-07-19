(* TEST
   (* Test that yielding between busy threads reliably triggers a thread
      switch. *)
   * hassysthreads
   include systhreads
   ** not-windows
   *** bytecode
   *** native
*)

let threads = 4

let are_ready = ref 0

let yields = ref 0

let iters = 50000

let last = ref (-1)

let report thread run_length =
  (* The below loop tests how many times in a row a loop that calls yield runs
     without changing threads. Ideally the answer would *always* be one, but
     it's not clear we can reliably guarantee that unless nothing else ever
     drops the Ocaml lock, so instead just rely on it being small. *)
  if run_length > 3
  then Printf.printf "Thread %d ran %d consecutive iters\n" thread run_length


let threads =
  List.init threads (Thread.create (fun i ->
    incr are_ready;
    (* Don't make any progress until all threads are spawned and properly
       contending for the Ocaml lock. *)
    while !are_ready < threads do
      Thread.yield ()
    done;
    let consecutive = ref 0 in
    while !yields < iters do
      incr yields;
      last := i;
      Thread.yield ();
      incr consecutive;
      if not (!last = i)
      then (
        report i !consecutive;
        consecutive := 0)
    done;
    if !consecutive > 0 then report i !consecutive;
  ));;

List.iter Thread.join threads
