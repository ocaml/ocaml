(* TEST
   modules = "aligned_alloc_stubs.c"
*)

(* benefit: will not collide with compaction, no noisy neighbour *)


external make_aligned : 'a -> 'a Atomic.t = "caml_make_aligned"
external is_aligned : 'a -> bool = "caml_is_aligned"

let test () = 
  let atomic = make_aligned 997 in 
  assert (Atomic.get atomic == 997);
  assert (is_aligned atomic)
;;

let negative_test () = 
  let l = List.init 100 (fun i -> ref i) in 
  let some_not_aligned =   
    List.exists (fun v -> not (is_aligned v)) l
  in
  assert some_not_aligned;
;;


let () = 
  test ();
  negative_test ();
;;


