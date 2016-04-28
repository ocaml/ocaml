(*

A testbed file for the module Format.

*)

open Testing;;

open Format;;

(* BR#4769 *)
let test0 () =
  let b = Buffer.create 10 in
  let msg = "Hello world!" in
  Format.bprintf b "%s" msg;
  let s = Buffer.contents b in
  s = msg
;;

test (test0 ())
;;
