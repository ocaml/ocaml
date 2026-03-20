(* TEST
 flags = "-i-variance";
 expect;
*)

(* external type PR; #13712 *)
(* external types with different names are provably different *)

type t = external "t";;
type u = external "u";;

let _ : (t, u) Type.eq -> _ = function _ -> .;;
[%%expect {|
type t = external "t"
type u = external "u"
- : (t, u) Type.eq -> 'a = <fun>
|}]

(* those with the same name could be equal *)

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

(* primitive abstract types are external each with its own name;
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


(* external types remain external seen from outside a module *)

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


(* tests for module inclusion *)

module M : sig
  type t = external "foo"
end = struct
  type t = external "foo"
end;;

[%%expect{|
module M : sig type t = external "foo" end
|}]

module M : sig
  type t
end = struct
  type t = external "foo"
end;;

[%%expect{|
module M : sig type t end
|}]

module M : sig
  type t = external "foo"
end = struct
  type t
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t end
       is not included in
         sig type t = external "foo" end
       Type declarations do not match:
         type t
       is not included in
         type t = external "foo"
       The first is abstract, but the second is external "foo".
|}]

module M : sig
  type t = external "foo"
end = struct
  type t = external "bar"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = external "bar"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = external "bar" end
       is not included in
         sig type t = external "foo" end
       Type declarations do not match:
         type t = external "bar"
       is not included in
         type t = external "foo"
       The first is external "bar", but the second is external "foo".
|}]


(* tests for variance and injectivity: all parameters are injective *)

type ('a, !'b, +'c, -'d, !+'e, !-'f) t = external "foo";;
type ('a, !'b, +'c, -'d, !+'e, !-'f) t2 = ('a, 'b, 'c, 'd, 'e, 'f) t;;
type 'a t3 = external "bar";;
type !'a t4 = 'a t3;;

[%%expect{|
type (!'a, !'b, +!'c, -!'d, +!'e, -!'f) t = external "foo"
type (!'a, !'b, +!'c, -!'d, +!'e, -!'f) t2 = ('a, 'b, 'c, 'd, 'e, 'f) t
type !'a t3 = external "bar"
type !'a t4 = 'a t3
|}]

type +'a t5 = 'a t3;;

[%%expect{|
Line 1, characters 0-19:
1 | type +'a t5 = 'a t3;;
    ^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

type -'a t6 = 'a t3;;

[%%expect{|
Line 1, characters 0-19:
1 | type -'a t6 = 'a t3;;
    ^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be contravariant,
       but it is injective invariant.
|}]


(* check for injectivity propagation through recursive modules *)

module rec M1 : sig
  type 'a t = external "foo"
end = struct
  type 'a t = external "foo"
end

and M2 : sig
  type !'b t
end = struct
  type 'b t = 'b M1.t
end;;

[%%expect{|
module rec M1 : sig type !'a t = external "foo" end
and M2 : sig type !'b t end
|}]


(* weird names are allowed *)

type t = external "";;

[%%expect{|
type t = external ""
|}]

type t = external "\003";;
[%%expect{|
type t = external "\003"
|}]

type t = external {a|\\\|a};;
[%%expect{|
type t = external "\\\\\\"
|}]
