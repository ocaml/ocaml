(* TEST
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* Test Thread.delay and its scheduling *)

open Printf

let tick (delay, count) =
  (* nanosleep is always permitted to suspend execution for _longer_ than the
     specified timeout. In particular, macOS seems to do this quite readily on
     Apple silicon (seen very readily in CI). We make some allowance for this by
     deducting additional sleep time from the next delay.
     The alternative is to time the C part of Thread.delay _precisely_ (in C),
     and then test whether both threads spend ~3 seconds in nanosleep, but this
     is a much more verbose test (and requires a C stub, so it doesn't strictly
     test Thread.delay at that point). *)
  let threshold = 1.05 *. delay in
  let rec loop delta =
    let start = Unix.gettimeofday () in
    Thread.delay (delay -. delta);
    let wallclock_delay = (Unix.gettimeofday () -. start) +. delta in
    incr count;
    let delta =
      if wallclock_delay >= threshold then
        (* Always call Thread.delay for at least 5% of the specified delay *)
        min (0.95 *. delay) (wallclock_delay -. delay)
      else
        delta
    in
    loop delta
  in
  loop 0.0

let _ =
  let c1 = ref 0 and c2 = ref 0 in
  ignore (Thread.create tick (0.333333333, c1));
  ignore (Thread.create tick (0.5, c2));
  Thread.delay 3.0;
  let n1 = !c1 and n2 = !c2 in
  if n1 >= 8 && n1 <= 10 && n2 >= 5 && n2 <= 7
  then printf "passed\n"
  else printf "FAILED (n1 = %d, n2 = %d)\n" n1 n2
