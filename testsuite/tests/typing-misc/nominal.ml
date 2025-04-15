(* TEST
 expect;
*)

(* nominal type PR; #13712 *)
(* nominal types with different names are provably different *)

type t = external "t";;
type u = external "u";;

let _ : (t, u) Type.eq -> _ = function _ -> .;;
[%%expect {|
type t = external "t"
type u = external "u"
- : (t, u) Type.eq -> 'a = <fun>
|}]

(* those with the same name are provably equal *)

type t' = external "t";;

let _ : (t, t') Type.eq -> _ = function _ -> .;;
[%%expect {|
type t' = external "t"
Line 3, characters 40-41:
3 | let _ : (t, t') Type.eq -> _ = function _ -> .;;
                                            ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "Equal"
|}]

(* primitive abstract types are nominal each with its own name;
   e.g., int is named "int" *)
let _ : (int, char) Type.eq -> _ = function _ -> .;;
[%%expect {|
- : (int, char) Type.eq -> 'a = <fun>
|}]

let _ : (string, bytes) Type.eq -> _ = function _ -> .;;
[%%expect {|
- : (string, bytes) Type.eq -> 'a = <fun>
|}]


(* equalities involving unnamed abstract types cannot be refuted *)

type v;;
type w;;

let _ : (v, t) Type.eq -> _ = function _ -> .;;
[%%expect {|
type v
type w
Line 4, characters 39-40:
4 | let _ : (v, t) Type.eq -> _ = function _ -> .;;
                                           ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "Equal"
|}]
let _ : (t, v) Type.eq -> _ = function _ -> .;;
[%%expect {|
Line 1, characters 39-40:
1 | let _ : (t, v) Type.eq -> _ = function _ -> .;;
                                           ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "Equal"
|}]
let _ : (v, int) Type.eq -> _ = function _ -> .;;
[%%expect {|
Line 1, characters 41-42:
1 | let _ : (v, int) Type.eq -> _ = function _ -> .;;
                                             ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "Equal"
|}]
let _ : (int, v) Type.eq -> _ = function _ -> .;;
[%%expect {|
Line 1, characters 41-42:
1 | let _ : (int, v) Type.eq -> _ = function _ -> .;;
                                             ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "Equal"
|}]
let _ : (v, w) Type.eq -> _ = function _ -> .;;
[%%expect {|
Line 1, characters 39-40:
1 | let _ : (v, w) Type.eq -> _ = function _ -> .;;
                                           ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "Equal"
|}]


(* nominal types remain nominal seen from outside a module *)

module M = struct
  type t
  type u
  type t' = external "t'"
  type u' = external "u'"
end;;

let _ : (M.t', M.u') Type.eq -> _ = function _ -> .;;
[%%expect {|
module M :
  sig type t type u type t' = external "t'" type u' = external "u'" end
- : (M.t', M.u') Type.eq -> 'a = <fun>
|}]

let _ : (M.t', int) Type.eq -> _ = function _ -> .;;
[%%expect {|
- : (M.t', int) Type.eq -> 'a = <fun>
|}]

let _ : (M.t', M.u) Type.eq -> _ = function _ -> .;;
[%%expect {|
Line 1, characters 44-45:
1 | let _ : (M.t', M.u) Type.eq -> _ = function _ -> .;;
                                                ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "Equal"
|}]

let _ : (M.t, M.u) Type.eq -> _ = function _ -> .;;
[%%expect {|
Line 1, characters 43-44:
1 | let _ : (M.t, M.u) Type.eq -> _ = function _ -> .;;
                                               ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "Equal"
|}]
let _ : (M.t, int) Type.eq -> _ = function _ -> .;;
[%%expect {|
Line 1, characters 43-44:
1 | let _ : (M.t, int) Type.eq -> _ = function _ -> .;;
                                               ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "Equal"
|}]
