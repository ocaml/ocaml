(* TEST
 expect;
*)

module type F = (type a) -> sig
  val default : a
end

module List (type a) = struct
  type t = a list
end

(* test alpha renaming *)
module List2 : (type b) -> sig
  type t = (b * b) list
end = functor (type a) -> struct
  type t = (a * a) list
end

[%%expect{|
module type F = (type a) -> sig val default : a end
module List : (type a) -> sig type t = a list end
module List2 : (type b) -> sig type t = (b * b) list end
|}]

(* Test valid applications *)

module IntList = List(type int)

module SumList = List(type [`A | `B])

[%%expect{|
module IntList : sig type t = int list end
module SumList : sig type t = [ `A | `B ] list end
|}]

(* Test all cases of wrong applications of modules for error messages *)

module Err1 = List(Int)

[%%expect{|
Line 1, characters 14-23:
1 | module Err1 = List(Int)
                  ^^^^^^^^^
Error: The functor expected a type argument at this position
|}]

module Err2 = List()

[%%expect{|
Line 1, characters 14-20:
1 | module Err2 = List()
                  ^^^^^^
Error: The functor expected a type argument at this position
|}]

module type T = sig
  type t
end

module Id (X : T) = X

[%%expect{|
module type T = sig type t end
module Id : (X : T) -> sig type t = X.t end
|}]

module Err3 = Id(type int)

[%%expect{|
Line 1, characters 14-26:
1 | module Err3 = Id(type int)
                  ^^^^^^^^^^^^
Error: The functor was expected to be applicative at this position
|}]

module G () = struct end

module Err4 = G(type int)

[%%expect{|
module G : () -> sig end
Line 3, characters 14-15:
3 | module Err4 = G(type int)
                  ^
Error: This is a generative functor. It can only be applied to "()"
|}]

(* Test coercions between types and related errors messages *)

module Swaping : (type a) (type b) -> sig
    type t = a
    type s = b
end = functor (type b) (type a) -> struct
    type t = b
    type s = a
end

[%%expect{|
module Swaping : (type a) (type b) -> sig type t = a type s = b end
|}]

module Err5 : (type t) -> sig type nonrec t = t end = Id

[%%expect{|
Line 1, characters 54-56:
1 | module Err5 : (type t) -> sig type nonrec t = t end = Id
                                                          ^^
Error: Signature mismatch:
       Modules do not match:
         (X : T) -> ...
       is not included in
         (type t) -> ...
       The functor expected a type argument at this position
|}]

module type Typ = sig type t end

module Err6 : (T : Typ) -> sig
  type t = (T.t * T.t) list
end = functor (type a) -> struct
  type t = (a * a) list
end

[%%expect{|
module type Typ = sig type t end
Lines 5-7, characters 14-3:
5 | ..............(type a) -> struct
6 |   type t = (a * a) list
7 | end
Error: Signature mismatch:
       Modules do not match:
         (type a) -> ...
       is not included in
         (T : Typ) -> ...
       The functor was expected to be applicative at this position
|}]



(* Test about applicativity of type application to a module *)

let f1 (x : List(type int).t) : List(type int).t = x

module M = List(type int)

let f2 (x : M.t) : List(type int).t = x

[%%expect{|
val f1 : List(type int).t -> List(type int).t = <fun>
module M : sig type t = int list end
val f2 : M.t -> List(type int).t = <fun>
|}]

let f_fail1 (x : List(type int).t) : List(type float).t = x

[%%expect{|
Line 1, characters 58-59:
1 | let f_fail1 (x : List(type int).t) : List(type float).t = x
                                                              ^
Error: The value "x" has type "List(type int).t" = "int list"
       but an expression was expected of type "List(type float).t" = "float list"
       Type "int" is not compatible with type "float"
|}]

(* Test error message if the type is a parametric type *)
let f_fail2 (x : List(type list).t) = x

[%%expect{|
Line 1, characters 17-32:
1 | let f_fail2 (x : List(type list).t) = x
                     ^^^^^^^^^^^^^^^
Error: The type constructor list expects 1 argument(s)
|}]

(* Tests error messages of invalid application in paths *)

let fail_in_path (x : List(Int).t) = x

[%%expect{|
Line 1, characters 22-31:
1 | let fail_in_path (x : List(Int).t) = x
                          ^^^^^^^^^
Error: The functor expected a type argument at this position
|}]

module type Typ = sig type t end

module IdTyp (T : Typ) = T

let fail_in_path2 (x : IdTyp(type int).t) = x

[%%expect{|
module type Typ = sig type t end
module IdTyp : (T : Typ) -> sig type t = T.t end
Line 5, characters 23-38:
5 | let fail_in_path2 (x : IdTyp(type int).t) = x
                           ^^^^^^^^^^^^^^^
Error: The functor was expected to be applicative at this position
|}]


(** Check that type-functors receive the same checks as applicative functors
  All the following tests go by two : one with a module argument and one with
  a type argument to check that both work the same way
*)

(* Preliminaries *)

module Gen () : Typ = struct type t = int end

[%%expect{|
module Gen : () -> Typ
|}]


(* No unpacking of first-class module in applicative functors *)

module F1app (T : Typ) = struct
  let m = (module T : Typ)
  module M = (val m)
end

[%%expect{|
Line 3, characters 13-20:
3 |   module M = (val m)
                 ^^^^^^^
Error: This expression creates fresh types.
       It is not allowed inside applicative functors.
|}]

module F1typ (type a) = struct
  module T = struct type t = a end
  let m = (module T : Typ)
  module M = (val m)
end

[%%expect{|
Line 4, characters 13-20:
4 |   module M = (val m)
                 ^^^^^^^
Error: This expression creates fresh types.
       It is not allowed inside applicative functors.
|}]

module F2app (T : Typ) = Gen ()

[%%expect{|
Line 1, characters 25-31:
1 | module F2app (T : Typ) = Gen ()
                             ^^^^^^
Error: This expression creates fresh types.
       It is not allowed inside applicative functors.
|}]

module F2typ (type a) = Gen ()

[%%expect{|
Line 1, characters 24-30:
1 | module F2typ (type a) = Gen ()
                            ^^^^^^
Error: This expression creates fresh types.
       It is not allowed inside applicative functors.
|}]

(* Here we check that we don't have a scope escape of 'a inside the path. *)
let id (type a) (x : List(type a).t) = x

[%%expect{|
val id : 'a list -> 'a list = <fun>
|}]


(* Test all error messages for all disallowed cases *)

module F_exception (type a) = struct
  exception E
end

[%%expect{|
Line 2, characters 2-13:
2 |   exception E
      ^^^^^^^^^^^
Error: This expression in not allowed in type functors.
|}]

type ext = ..

module F_typeext (type a) = struct
  type ext += C
end

[%%expect{|
type ext = ..
Line 4, characters 2-15:
4 |   type ext += C
      ^^^^^^^^^^^^^
Error: This expression in not allowed in type functors.
|}]

module F_typeext2 (type a) = struct
  type ext2 = ..
  type ext2 += C2
end

[%%expect{|
Line 3, characters 2-17:
3 |   type ext2 += C2
      ^^^^^^^^^^^^^^^
Error: This expression in not allowed in type functors.
|}]

module F_value (type a) = struct
  print_newline ()
end

[%%expect{|
Line 2, characters 2-18:
2 |   print_newline ()
      ^^^^^^^^^^^^^^^^
Error: This expression in not allowed in type functors.
|}]

(* Could be allowed using relaxed value restriction *)
module F_let1 (type a) = struct
  let f = (fun x -> x) (fun () -> ())
end

[%%expect{|
Line 2, characters 2-37:
2 |   let f = (fun x -> x) (fun () -> ())
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression in not allowed in type functors.
|}]

(* Could be allowed if the type of f is defined before F_let2 *)
module F_let2 (type a) = struct
  let f = ref None
end

[%%expect{|
Line 2, characters 2-18:
2 |   let f = ref None
      ^^^^^^^^^^^^^^^^
Error: This expression in not allowed in type functors.
|}]

module F_let3 (type a) = struct
  let f : a option ref = ref None
end

[%%expect{|
Line 2, characters 2-33:
2 |   let f : a option ref = ref None
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression in not allowed in type functors.
|}]

module F_class (type a) = struct
  class c = object
  end
end

[%%expect{|
Lines 2-3, characters 2-5:
2 | ..class c = object
3 |   end
Error: This expression in not allowed in type functors.
|}]
