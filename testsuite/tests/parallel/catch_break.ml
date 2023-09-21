(* TEST
hassysthreads;
include systhreads;
not-windows;
{
  bytecode;
}{
  native;
}
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

let print = if verbose then print_endline else fun _ -> ()

let break_trap s =
  begin
    try while true do () done
    with Sys.Break -> print "[Sys.Break caught]";
  end;
  print s

let run () =
  (* Goal: joining the domain [d] must be achievable by Ctrl-C *)
  let d = Domain.spawn (fun () -> break_trap "Domain 1")
  in
  let finished = Atomic.make false in
  (* Simulate repeated Ctrl-C *)
  let d2 = Domain.spawn (fun () ->
    ignore (Thread.sigmask Unix.SIG_BLOCK [Sys.sigint]);
    let pid = Unix.getpid () in
    let rec kill n =
      if n = 0 then (
        print "[Kill thread reached max attempts without succeeding]";
        Unix._exit 1
      );
      Unix.sleepf 0.05;
      Unix.kill pid Sys.sigint;
      if not (Atomic.get finished) then kill (n - 1)
    in
    kill 10)
  in
  break_trap "Domain 0 - 1";
  Domain.join d;
  break_trap "Domain 0 - 2";
  Atomic.set finished true;
  Domain.join d2

let () =
  Sys.catch_break true;
  (try run () with Sys.Break -> ());
  (try print "Success." with Sys.Break -> ());
  exit 0
