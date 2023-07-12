(* TEST

 include unix;
 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 native;

*)
let () =
  let v = Array.make 4 0 in
  let t1 = Domain.spawn (fun () -> Array.set v 3 0; Unix.sleepf 0.1) in
  let t2 = Domain.spawn (fun () -> ignore (Sys.opaque_identity (Array.get v 3)); Unix.sleepf 0.1) in
  Domain.join t1;
  Domain.join t2;
