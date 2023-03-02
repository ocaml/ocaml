(* TEST

 include unix;
 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 native;

*)

let v = Atomic.make 0

let () =
  let t1 = Domain.spawn (fun () -> Atomic.set v 10; Unix.sleep 1) in
  let t2 = Domain.spawn (fun () ->
      ignore (Sys.opaque_identity (Atomic.get v)); Unix.sleep 1) in
  Domain.join t1;
  Domain.join t2
