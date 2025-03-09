let x = B.x + 1
let y = A.x + 1

(* Typing x requires loading A's cmi.  When it is made available with -H, y
   should fail to typecheck because direct references to A are not allowed, even
   though it has been loaded. *)
