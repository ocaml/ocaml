(* TEST
*)

open Gc

let min_heap_sz = 524288 (* 512k *)
let maj_heap_inc = 4194304 (* 4M *)

let _ =
  let g1 = Gc.get() in
  Gc.set { g1 with minor_heap_size = min_heap_sz;
                   major_heap_increment = maj_heap_inc };
  let g2 = Gc.get() in
  assert (g2.minor_heap_size = min_heap_sz);
  assert (g2.major_heap_increment = maj_heap_inc);
  assert(g2.space_overhead = g1.space_overhead);
  assert(g2.max_overhead = g1.max_overhead);
  assert(g2.stack_limit = g1.stack_limit);
  assert(g2.allocation_policy = g1.allocation_policy);
  assert(g2.window_size = g1.window_size);
  assert(g2.custom_major_ratio = g1.custom_major_ratio);
  assert(g2.custom_minor_ratio = g1.custom_minor_ratio);
  assert(g2.custom_minor_max_size = g1.custom_minor_max_size)

  
