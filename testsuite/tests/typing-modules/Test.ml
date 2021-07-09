(* TEST
   * expect
*)

(* with module *)

module type S = sig type t and s = t end;;
module type S' = S with type t := int;;
[%%expect{|
module type S = sig type t and s = t end
module type S' = sig type s = int end
|}];;

module type S = sig module rec M : sig end and N : sig end end;;
module type S' = S with module M := String;;
[%%expect{|
module type S = sig module rec M : sig end and N : sig end end
module type S' = sig module rec N : sig end end
|}];;

(* with module type *)
(*
module type S = sig module type T module F(X:T) : T end;;
module type T0 = sig type t end;;
module type S1 = S with module type T = T0;;
module type S2 = S with module type T := T0;;
module type S3 = S with module type T := sig type t = int end;;
module H = struct
  include (Hashtbl : module type of Hashtbl with
           type statistics := Hashtbl.statistics
           and module type S := Hashtbl.S
           and module Make := Hashtbl.Make
           and module MakeSeeded := Hashtbl.MakeSeeded
           and module type SeededS := Hashtbl.SeededS
           and module type HashedType := Hashtbl.HashedType
           and module type SeededHashedType := Hashtbl.SeededHashedType)
end;;
*)

(* A subtle problem appearing with -principal *)
type -'a t
class type c = object method m : [ `A ] t end;;
module M : sig val v : (#c as 'a) -> 'a end =
  struct let v x = ignore (x :> c); x end;;
[%%expect{|
type -'a t
class type c = object method m : [ `A ] t end
module M : sig val v : (#c as 'a) -> 'a end
|}];;

(* PR#4838 *)

let id = let module M = struct end in fun x -> x;;
[%%expect{|
val id : 'a -> 'a = <fun>
|}];;

(* PR#4511 *)

let ko = let module M = struct end in fun _ -> ();;
[%%expect{|
val ko : 'a -> unit = <fun>
|}];;

(* PR#5993 *)

module M : sig type -'a t = private int end =
  struct type +'a t = private int end
;;
[%%expect{|
Line 2, characters 2-37:
2 |   struct type +'a t = private int end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type +'a t = private int end
       is not included in
         sig type -'a t = private int end
       Type declarations do not match:
         type +'a t = private int
       is not included in
         type -'a t = private int
       Their variances do not agree.
|}];;

(* PR#6005 *)

module type A = sig type t = X of int end;;
type u = X of bool;;
module type B = A with type t = u;; (* fail *)
[%%expect{|
module type A = sig type t = X of int end
type u = X of bool
Line 3, characters 23-33:
3 | module type B = A with type t = u;; (* fail *)
                           ^^^^^^^^^^
Error: This variant or record definition does not match that of type u
       Constructors do not match:
         X of bool
       is not the same as:
         X of int
       The type bool is not equal to the type int
|}];;

(* PR#5815 *)
(* ---> duplicated exception name is now an error *)

module type S = sig exception Foo of int  exception Foo of bool end;;
[%%expect{|
Line 1, characters 42-63:
1 | module type S = sig exception Foo of int  exception Foo of bool end;;
                                              ^^^^^^^^^^^^^^^^^^^^^
Error: Multiple definition of the extension constructor name Foo.
       Names must be unique in a given structure or signature.
|}];;

(* PR#6410 *)

module F(X : sig end) = struct let x = 3 end;;
F.x;; (* fail *)
[%%expect{|
module F : functor (X : sig end) -> sig val x : int end
Line 2, characters 0-3:
2 | F.x;; (* fail *)
    ^^^
Error: The module F is a functor, it cannot have any components
|}];;

type t = ..;;
[%%expect{|
type t = ..
|}];;

module M : sig type t += E end = struct type t += E of int end;;
[%%expect{|
Line 1, characters 33-62:
1 | module M : sig type t += E end = struct type t += E of int end;;
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t += E of int end
       is not included in
         sig type t += E end
       Extension declarations do not match:
         type t += E of int
       is not included in
         type t += E
       Constructors do not match:
         E of int
       is not the same as:
         E
       They have different arities.
|}];;

module M : sig type t += E of char end = struct type t += E of int end;;
[%%expect{|
Line 1, characters 41-70:
1 | module M : sig type t += E of char end = struct type t += E of int end;;
                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t += E of int end
       is not included in
         sig type t += E of char end
       Extension declarations do not match:
         type t += E of int
       is not included in
         type t += E of char
       Constructors do not match:
         E of int
       is not the same as:
         E of char
       The type int is not equal to the type char
|}];;

module M : sig type t += C of int end = struct type t += E of int end;;
[%%expect{|
Line 1, characters 40-69:
1 | module M : sig type t += C of int end = struct type t += E of int end;;
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t += E of int end
       is not included in
         sig type t += C of int end
       The extension constructor `C' is required but not provided
|}];;

module M : sig
  type t += E of { x : int }
end = struct
  type t += E of int
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t += E of int
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t += E of int end
       is not included in
         sig type t += E of { x : int; } end
       Extension declarations do not match:
         type t += E of int
       is not included in
         type t += E of { x : int; }
       Constructors do not match:
         E of int
       is not the same as:
         E of { x : int; }
       The second uses inline records and the first doesn't.
|}];;
