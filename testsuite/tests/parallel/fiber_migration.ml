(* TEST
 multicore;
 {
   bytecode;
 }{
   native;
 }
*)

(** Test fiber migration. We test that sending fibers from one domain
    to another do not increase the size of the fiber cache of the
    receiving domain in an unbounded way *)

(** Migration effect *)
type _ Effect.t += Migrate : unit Effect.t

(** Transfer of continuation to a shared stack *)
let shared_fiber_stack: (unit->unit) list Atomic.t = Atomic.make []
let rec push item =
  (* Update to Atomic.update once the function is less fresh *)
   let before = Atomic.get shared_fiber_stack in
   let after = item :: before in
   if not (Atomic.compare_and_set shared_fiber_stack before after) then
     push item
let get_fibers () = Atomic.get shared_fiber_stack
let take_fibers () = List.rev (Atomic.exchange shared_fiber_stack [])

(** Produce fibers by performing the Migrate effect, then store
    them in the shared stack *)
let producer_loop () =
  let max_live_fiber = 10 in
  for _ = 1 to max_live_fiber do
    match Effect.perform Migrate with
    | () -> ()
    | effect Migrate, k -> push (Effect.Deep.continue k)
  done;
  (* Wait for the consumer to cache all fibers in order to bound the
     number of live fibers *)
  while get_fibers () <> [] do Domain.cpu_relax () done

(** Receiver loop: pick fibers in the shared stack, resume them and
    store them in the cache of the receiver domain. *)
let rec receiver_loop = function
  | [] ->
      (* wait for the producer to send more fibers *)
      while get_fibers () = [] do
        Domain.cpu_relax ()
      done;
      receiver_loop (take_fibers ())
  | fiber :: fibers ->
      (* we resume the fiber in the shared stack, and store it in the
         current domain cache *)
      match fiber () with
      | () -> receiver_loop fibers
      | exception Exit -> ()

let main () =
  let receiving_domain = Domain.spawn (fun () -> receiver_loop []) in
  let finally () =
    push (fun () -> raise Exit);
    Domain.join receiving_domain
  in
  Fun.protect ~finally begin fun () ->
    for _ = 1 to 50 do producer_loop () done;
    (* Check that the size of fiber caches did not grow unboundedly during the loop *)
    let cache_size = (Gc.quick_stat()).live_stacks_words in
    if (cache_size < 25000) then
      Printf.printf "OK\n%!"
    else Printf.printf "Bad %d\n%!" cache_size
  end

let () = main ()
