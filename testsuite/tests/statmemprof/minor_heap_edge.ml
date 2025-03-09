(* TEST *)

module MP = Gc.Memprof

(* This is a stress-test for weird behaviour when the minor heap is just about to
   overflow, which is easier to trigger when the minor heap is small *)
let () =
  Gc.set { (Gc.get ()) with minor_heap_size = 2000 }

let f () =
  let n_allocated = ref 0 in
  let n_promoted = ref 0 in
  let n_deallocated = ref 0 in
  let _:MP.t =
    let alloc_minor _info =
      incr n_allocated;
      for i = 1 to Random.int 500 do
        ignore (Sys.opaque_identity (ref 42))
      done;
      Some ()
    in
    let promote () =
      incr n_promoted;
      None
    in
    let dealloc_minor () =
      incr n_deallocated;
      ()
    in
    MP.start ~callstack_size:0 ~sampling_rate:1.
      { MP.null_tracker with alloc_minor; promote; dealloc_minor }
  in
  let r = ref 42 in
  let s = ref [] in
  for i = 1 to 10_000 do
    incr r;
    (* This is a largeish, combined, non-constant allocation,
       so goes through caml_memprof_track_young *)
    s := [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
            0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
            0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
            0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
            0; 0; 0; 0; 0; 0; 0; 0; 0; !r; |] :: !s;
  done;
  (* make sure all values are promoted *)
  Gc.full_major ();
  ignore (Sys.opaque_identity !s);
  (* make sure all values are collected *)
  s := [];
  Gc.full_major ();
  ignore (Sys.opaque_identity !s);
  MP.stop ();
  Printf.printf "%d %d %d\n" !n_allocated !n_promoted !n_deallocated;
  ()

let () = f ()
