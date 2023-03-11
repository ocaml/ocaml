(* TEST
   include unix
   * libunix
   ** bytecode
   ** native
 *)

let () = Random.init 42

let domain_count = 10

let delays =
  (* These Random calls are intentionally:
   - taken from an independent Random state, not the global state we are testing
   - initialized with make_self_init, to return different delays on each tst run
  *)
  let delay_rng = Random.State.make_self_init () in
  List.init domain_count (fun _i -> Random.State.float delay_rng 0.5)

(* Each domain will start by waiting a random amount, to ensure that
   the Random.int functions we are testing execute in
   non-deterministic order. The Random.int result should remain
   deterministic, as domains are spawned in a deterministic order and
   each domain state is obtaind by splitting the global Random state
   that was initialized with a fixed seed. *)
let f delay () =
  Unix.sleepf delay;
  let a = Random.int 100 in
  let b = Random.int 100 in
  let c = Random.int 100 in
  (a, b, c)

let () =
  delays
  |> List.map (fun delay -> Domain.spawn (f delay))
  |> List.map Domain.join
  |> List.iter (fun (a, b, c) -> Printf.printf "%d %d %d\n%!" a b c)

let () =
  print_endline
    "Note: we observe in this output that the random numbers of each child domain\n\
     appear uncorrelated, yet are produced deterministically."
