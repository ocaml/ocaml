(* TEST

 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 readonly_files = "waitgroup_stubs.c";
 all_modules = "${readonly_files} waitgroup.ml record_field.ml";
 native;

*)
type t = { mutable x : int }

let wg1 = Waitgroup.create 2
let wg2 = Waitgroup.create 2
let v = { x = 0 }

let writer () =
  v.x <- 10;
  Waitgroup.join wg1;
  Waitgroup.join wg2

let reader () =
  Waitgroup.join wg1;
  ignore (Sys.opaque_identity v.x);
  Waitgroup.join wg2

let () =
  let d = Domain.spawn writer in
  reader ();
  Domain.join d
