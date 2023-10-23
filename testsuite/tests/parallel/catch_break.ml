(* TEST
hassysthreads;
include systhreads;
not-windows;
no-tsan;
{
  bytecode;
}{
  native;
}
*)

(* PR #11307. The following program deadlocks when input in the
   toplevel and interrupted by the user with Ctrl-C, by busy-waiting
   on signals to be processed.

   {[
let break_trap s =
  (try while true do () done
   with Sys.Break -> print_endline "[Sys.Break caught]" ) ;
  print_endline s

let () =
  Sys.catch_break true ;
  let d = Domain.spawn (fun () -> break_trap "Domain 1") in
  break_trap "Domain 0 - 1" ;
  Domain.join d ;
  break_trap  "Domain 0 - 2";
  print_endline "Success."
   ]}

*)

let verbose = false

(* Expected when verbose (depending on scheduling and platform):

[Sys.Break caught]
Domain 1
[Sys.Break caught]
Domain 0 - 1
[Sys.Break caught]
Domain 0 - 2
Success.

*)

let delay = 0.001 (* 1 ms *)
let fuel = Atomic.make 1000 (* = 1s max retry duration *)

let print = if verbose then print_endline else fun _ -> ()

(* start sending interrupts when reaches 1 or 2 *)
let ready_count = Atomic.make 0

(* Does not poll *)

let sleep () =
  if Atomic.get fuel <= 0 then (
    print "[Reached max attempts without succeeding]";
    Unix._exit 1
  );
  Atomic.decr fuel;
  Unix.sleepf delay

let rec wait n =
  if Atomic.get ready_count <> n then (
    sleep ();
    wait n
  )

(* We busy-wait because other synchronisation mechanisms involve
   blocking calls, which may exercise other parts of the async
   callback implementation than we want.*)
let break_trap s =
  begin
    try Atomic.incr ready_count; while true do () done
    with Sys.Break -> print "[Sys.Break caught]"
  end;
  print s;
  Atomic.decr ready_count

(* Simulate repeated Ctrl-C from a parallel thread *)
let interruptor_domain () =
  Domain.spawn @@ fun () ->
  ignore (Thread.sigmask Unix.SIG_BLOCK [Sys.sigint]);
  let kill () = sleep () ; Unix.kill (Unix.getpid ()) Sys.sigint in
  wait 2;
  kill (); (* interrupt Domain 1 or Domain 0-1 *)
  wait 1;
  kill (); (* interrupt the other one of Domain 1 or Domain 0-1 *)
  wait 2;
  kill ()  (* interrupt Domain 0-2 *)

let run () =
  (* We simulate the user pressing Ctrl-C repeatedly. Goal: joining
     the domain [d] must be achievable by Ctrl-C. This tests proper
     reception of SIGINT. *)
  let d = Domain.spawn (fun () -> break_trap "Domain 1") in
  let d2 = interruptor_domain () in
  break_trap "Domain 0 - 1";
  Domain.join d;
  assert (Atomic.get ready_count = 0);
  Atomic.incr ready_count; (* Make sure it reaches 2 *)
  break_trap "Domain 0 - 2";
  Domain.join d2

let () =
  Sys.catch_break true;
  (try run () with Sys.Break ->
     print ("Test could not complete due to scheduling hazard"
            ^ " (possible false positive)."));
  print "Success.";
  exit 0
