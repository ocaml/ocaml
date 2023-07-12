(* TEST

 include unix;
 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 native;

*)
type t = { mutable x : int }

let v = { x = 0 }

let () =
  let t1 = Domain.spawn (fun () -> v.x <- 10; Unix.sleepf 0.1) in
  let t2 = Domain.spawn (fun () -> ignore (Sys.opaque_identity v.x); Unix.sleepf 0.1) in
  Domain.join t1;
  Domain.join t2
