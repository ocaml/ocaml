(* TEST
   * expect
*)
type t = { x : int; self : t };;
[%%expect {|
type t = { x : int; self : t; }
|}];;

let rec x = 1
and u = Some { t with x = 2 }
and t = { x; self = t }
(* We have carefully placed `u` before `t` here,
   so that the copy { t with .. }, if accepted,
   is evaluated before 't' is initialized -- making
   the assertion below fail, typically aborting
   with a segmentation fault.

   If you exchange the declaration orders of `u` and `t`,
   and the static check accepts this example, then `t`
   is initialized first and the assertion succeeds. *)


let () = match u with
  | None -> assert false
  | Some {x = _; self} -> assert (self.x = t.x)
[%%expect {|
Line 2, characters 8-29:
2 | and u = Some { t with x = 2 }
            ^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;
