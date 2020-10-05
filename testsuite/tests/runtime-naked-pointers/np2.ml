(* TEST
   modules = "cstubs.c np.ml"
   * bytecode
   * native
*)

open Np

(* Out-of-heap object with black header is accepted even in no-naked-pointers
   mode.  GC doesn't scan black objects.  However, if the size in the
   head is crazily big, the naked pointer detector will warn. *)

let x = do_gc [ make_block (-1n) Black 100n ]
