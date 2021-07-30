(* TEST
   modules = "cstubs.c np.ml"
   * naked_pointers
   ** bytecode
   ** native
*)

open Np

(* Out-of-heap object with non-black header is OK in naked pointers mode only *)
(* Note that the header size can be wrong as it should not be used by the GC *)

let x = do_gc [ make_block 10000n White 10n;
                make_block 1n Blue 0n;
                make_block (-1n) Gray 5n ]
