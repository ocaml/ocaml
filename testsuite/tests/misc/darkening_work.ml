(* TEST
 { native; }
*)

(* This test checks that the extra marking work done by caml_modify is
   correctly accounted towards the current GC cycle, by checking that
   no-op writes do not cause GC cycles to complete more quickly than
   they should.

   (This test is native-only because it's a bit slow, and it's not
   testing anything different in bytecode)
*)

let table = Array.init 10_000 (fun _ -> Bytes.make 1_000 'a')

let major_cycles () = (Gc.quick_stat ()).major_collections

let[@inline never] f ~writes =
  List.init (Array.length table) (fun i ->
    let buf = table.(i) in
    if Sys.opaque_identity writes then table.(i) <- buf;
    Bytes.get buf 42)

let count_cycles ~f =
  Gc.major ();
  let before = major_cycles () in
  for i = 1 to 10_000 do
    ignore (Sys.opaque_identity (f ()))
  done;
  let after = major_cycles () in
  after - before

let () =
  (* wait until we do a few cycles to let the heap grow to a steady state *)
  let cyc_before = major_cycles () in
  while major_cycles () < cyc_before + 4 do
    ignore (Sys.opaque_identity (f ~writes:false))
  done;
  let wfalse = count_cycles ~f:(fun () -> f ~writes:false) in
  let wtrue = count_cycles ~f:(fun () -> f ~writes:true) in
  let wdiff = wtrue - wfalse in
  if abs wdiff > 2 then
    Printf.printf "error: writes caused %d more cycles\n" wdiff
  else
    Printf.printf "ok\n"
