(* TEST
*)

(* tests that we correctly empty the shared pool's freelist. This requires a
    bunch of garbage to be generated, a major cycle and two compactions to
    test. If we can do that without segfault, we're good. *)

let () =
  let arr = ref (Some (Array.init 1000000 (fun x -> (Some x)))) in
    Gc.minor ();
    (* Now arr should be promoted to the major heap *)
    arr := None;
    Gc.full_major ();
    (* Now arr should be garbage and the pools in the shared heap allocated
      for it should be on the free list *)
    Gc.compact ();
    (* Now the pools should be compacted but also the freelist should have
       been reset correctly *)
    Gc.compact ()
