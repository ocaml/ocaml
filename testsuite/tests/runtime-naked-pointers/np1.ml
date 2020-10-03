(* TEST
   modules = "cstubs.c np.ml"
   * bytecode
   * native
*)

open Np

(* Out-of-heap object with black header is accepted even in no-naked-pointers
   mode.  GC doesn't scan black objects. *)

let x = do_gc [ make_block 100n Black 100n ]
