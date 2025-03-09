(* TEST

 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 readonly_files = "waitgroup_stubs.c";
 all_modules = "${readonly_files} waitgroup.ml norace_atomics.ml";
 native;

*)

let wg = Waitgroup.create 2
let v = Atomic.make 0

let [@inline never] writer () =
  Waitgroup.join wg;
  Atomic.set v 10

let [@inline never] reader () =
  ignore (Sys.opaque_identity (Atomic.get v));
  Waitgroup.join wg

let () =
  let d = Domain.spawn writer in
  reader ();
  Domain.join d
