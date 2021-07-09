(* TEST *)

(* Example from PR #10325.
   This triggered a segfault in bytecode, but only if the code was not compiled
   in debug mode (the offending code is actually in camlinternalOO.ml, and is
   used only when optimising).
 *)

let x = object  method g = "abc" end
let s =  (object method f = x#g end)#f
let () = prerr_endline s
