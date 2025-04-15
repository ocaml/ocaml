(* TEST
 flags = "-i-variance";
 expect;
*)

(* more tests on Type_nominal (#13712) from Richard's experiment *)

type t1 = external "t1";;
type t2 = external "t2";;
type floaty = external "%float";;

[%%expect{|
type t1 = external "t1"
type t2 = external "t2"
type floaty = external "%float"
|}]

let f : t1 -> t2 = fun x -> x;;

[%%expect{|
Line 1, characters 28-29:
1 | let f : t1 -> t2 = fun x -> x;;
                                ^
Error: The value "x" has type "t1" but an expression was expected of type "t2"
|}]

type t1' = external "t1";;

let f : t1 -> t1' = fun x -> x;;

[%%expect{|
type t1' = external "t1"
Line 3, characters 29-30:
3 | let f : t1 -> t1' = fun x -> x;;
                                 ^
Error: The value "x" has type "t1" but an expression was expected of type "t1'"
|}]

let f : floaty -> float = fun x -> x;;

[%%expect{|
Line 1, characters 35-36:
1 | let f : floaty -> float = fun x -> x;;
                                       ^
Error: The value "x" has type "floaty" but an expression was expected of type
         "float"
|}]

(* check that the compiler recognizes floaty as a float type by
   checking for the float-record optimization *)
(* tag will be 254 when optimized; it is the desired result *)

external float_to_floaty : float -> floaty = "%identity";;

type r = { x1 : float; x2 : floaty; x3 : float };;

let tag = Obj.tag (Obj.repr { x1 = 3.14; x2 = float_to_floaty 5.68; x3 = 2.78 });;

[%%expect{|
external float_to_floaty : float -> floaty = "%identity"
type r = { x1 : float; x2 : floaty; x3 : float; }
val tag : int = 0
|}]

(* similar, but for non-float *)

type r = { x1 : float; x2 : t1; x3 : float };;

let tag = Obj.tag (Obj.repr { x1 = 3.14; x2 = Obj.magic (); x3 = 2.78 });;

[%%expect{|
type r = { x1 : float; x2 : t1; x3 : float; }
val tag : int = 0
|}]

(* check that non-float external types skip the flat-float array optimization *)

let arr : t1 array = [| Obj.magic 3.14 |];;
let tag = Obj.tag (Obj.repr arr);;

[%%expect{|
val arr : t1 array = [|<external>|]
val tag : int = 254
|}]

(*
(* This feature exists in Richard's experiment, but not in #13712. *)
(* This will be [type floaty2 = floaty = external "%float"] someday *)

type floaty2 = external ("%float" : floaty)

let f : floaty -> floaty2 = fun x -> x

[%%expect{|
type floaty2 = external ("%float" : floaty)
val f : floaty -> floaty2 = <fun>
|}]
*)

(* No private external types *)

(*
type t4 = private external "boo"

[%%expect{|
Line 1, characters 18-26:
1 | type t4 = private external "boo"
                      ^^^^^^^^
Error: Syntax error
|}]
*)

(* No unboxed external types *)

type t5 = external "foo" [@@unboxed];;

[%%expect{|
Line 1, characters 0-36:
1 | type t5 = external "foo" [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because it is abstract.
|}]

(* test module inclusion *)

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
       The first is abstract, but the second is nominal "foo".
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
       The first is nominal "bar", but the second is nominal "foo".
|}]

module M : sig
  type t = external "%float"
end = struct
  type t = external "%float"
end;;

[%%expect{|
module M : sig type t = external "%float" end
|}]

module M : sig
  type t
end = struct
  type t = external "%float"
end;;

[%%expect{|
module M : sig type t end
|}]

module M : sig
  type t = external "%int"
end = struct
  type t = external "%float"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = external "%float"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = external "%float" end
       is not included in
         sig type t = external "%int" end
       Type declarations do not match:
         type t = external "%float"
       is not included in
         type t = external "%int"
       The first is nominal "%float", but the second is nominal "%int".
|}]

(* Cannot use "external" in a [with] constraint *)

(*
module type S = sig
  type t
end

module type S2 = S with type t = external "%float"

[%%expect{|
module type S = sig type t end
Line 5, characters 33-41:
1 | module type S2 = S with type t = external "%float"
                                     ^^^^^^^^
Error: Syntax error
|}]
*)

(* Test variance and injectivity: all parameters are injective, but
   variance must be declared *)

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

(*
(* This feature exists in Richard's experiment, but not in #13712. *)
(* check for manifests *)

type t8 = external "foo"
type t9 = external ("foo" : t8)

[%%expect{|
type t8 = external "foo"
type t9 = external ("foo" : t8)
|}]

type t10 = external ("bar" : t8)

[%%expect{|
Line 1, characters 0-35:
1 | type t10 = external ("bar" : t8)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type t8
       The original is a primitive type "foo", but this is a primitive type "bar".
|}]

type t11 = external ("%float" : float)

[%%expect{|
type t11 = external ("%float" : float)
|}]

(* check for builtin *)

type bad = external "%bad"

[%%expect{|
Line 1, characters 0-29:
1 | type bad = [%external "%bad"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Malformed external type: Unknown primitive %bad
|}]
*)

(* check for empty name; allowed in #13712 *)

type t = external "";;

[%%expect{|
type t = external ""
|}]
