(* TEST *)

(* This test catches a bug in the 4.x compactor that would happen when
   the live data on the major heap consumes exactly one "chunk", with
   no free space: https://github.com/ocaml/ocaml/issues/9853

   The most robust reproduction case for this issue would be as follows:
   {[
   let c = ref []

   let () =
     for i = 0 to 1000000 do
       c := 0 :: !c;
       Gc.compact ()
    done
   ]}

   This fills the heap with very small words, and is likely to
   eventually reach exactly the right size condition. Unfortunately it
   also takes ages: on a test machine it takes 1m1s to reach the bug
   on OCaml 4.08. This is not reasonable in a CI setting, and using
   a smaller round counter in fact hides the issue as the chunk never
   gets filled.

   To get a faster test, our strategy is to bet that trouble arise
   when the live size is close to a multiple of 4096 words, or 32KiB:
   all chunk sizes used by the 4.x GC are multiples of this size. When
   we are far from a 4096-word limit, we allocate larger blocks to go
   faster. When we are close to the limit (before and after) we
   allocate very small blocks.

   On a test machine, this segfaults in 0.6s on a buggy 4.x runtime, and
   passes in 2.5s on a non-buggy runtime.
*)

let c = ref []

let live_words () =
  Gc.full_major ();
  (Gc.quick_stat ()).Gc.live_words

let size = ref (live_words ())

let allocate target_whsz =
  let array_header_wsz = 1 in
  let cons_cell_whsz = 3 in
  let len = max 0 (target_whsz - array_header_wsz - cons_cell_whsz) in
  c := Array.make len 42 :: !c;
  size := live_words ()

let test_around target_heap_size =
  let max_size = 128 in
  (* we don't use blocks of total size above 128, as the 5.x runtime
     would allocate them with 'malloc' which does not test compaction
     at all. *)
  let buffer = 64 in
  while !size < target_heap_size + buffer do
    let distance = max 1 (target_heap_size - !size) in
    allocate (min max_size (distance / 4));
    Gc.compact ();
  done

let () =
  let test i =
    print_int i; print_newline ();
    test_around (i * 4096)
  in
  for i = 1 to 32 do
    test i;
  done
