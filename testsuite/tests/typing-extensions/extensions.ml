(* TEST
   * expect
*)

(* Ignore OCAMLRUNPARAM=b to be reproducible *)
Printexc.record_backtrace false;;
[%%expect {|
- : unit = ()
|}]

type foo = ..
;;
[%%expect {|
type foo = ..
|}]

type foo +=
    A
  | B of int
;;
[%%expect {|
type foo += A | B of int
|}]

let is_a x =
  match x with
    A -> true
  | _ -> false
;;
[%%expect {|
val is_a : foo -> bool = <fun>
|}]

(* The type must be open to create extension *)

type foo
;;
[%%expect {|
type foo
|}]

type foo += A of int
;;
[%%expect {|
Line 1, characters 0-20:
1 | type foo += A of int
    ^^^^^^^^^^^^^^^^^^^^
Error: Type definition foo is not extensible
|}]

(* The type must be public to create extension *)

type foo = private ..
;;
[%%expect {|
type foo = private ..
|}]

type foo += A of int
;;
[%%expect {|
Line 1, characters 12-20:
1 | type foo += A of int
                ^^^^^^^^
Error: Cannot extend private type definition foo
|}]

(* The type parameters must match *)

type 'a foo = ..
;;
[%%expect {|
type 'a foo = ..
|}]

type ('a, 'b) foo += A of int
;;
[%%expect {|
Line 1, characters 0-29:
1 | type ('a, 'b) foo += A of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This extension does not match the definition of type foo
       They have different arities.
|}]

(* In a signature the type can be private *)

module type S =
sig
  type foo = private ..
  type foo += A of float
end
;;
[%%expect {|
module type S = sig type foo = private .. type foo += A of float end
|}]

(* But it must still be extensible *)

module type S =
sig
  type foo
  type foo += B of float
end
;;
[%%expect {|
Line 4, characters 2-24:
4 |   type foo += B of float
      ^^^^^^^^^^^^^^^^^^^^^^
Error: Type definition foo is not extensible
|}]

(* Signatures can change the grouping of extensions *)

type foo = ..
;;
[%%expect {|
type foo = ..
|}]

module M = struct
  type foo +=
      A of int
    | B of string

  type foo +=
      C of int
    | D of float
end
;;
[%%expect {|
module M :
  sig
    type foo += A of int | B of string
    type foo += C of int | D of float

  end
|}]

module type S = sig
  type foo +=
      B of string
    | C of int

  type foo += D of float

  type foo += A of int
end
;;
[%%expect {|
module type S =
  sig
    type foo += B of string | C of int
    type foo += D of float
    type foo += A of int
  end
|}]

module M_S = (M : S)
;;
[%%expect {|
module M_S : S
|}]

(* Extensions can be GADTs *)

type 'a foo = ..
;;
[%%expect {|
type 'a foo = ..
|}]

type _ foo +=
    A : int -> int foo
  | B : int foo
;;
[%%expect {|
type _ foo += A : int -> int foo | B : int foo
|}]

let get_num : type a. a foo -> a -> a option = fun f i1 ->
    match f with
        A i2 -> Some (i1 + i2)
     |  _ -> None
;;
[%%expect {|
val get_num : 'a foo -> 'a -> 'a option = <fun>
|}]

(* Extensions can have inline records (regression test for #9970) *)
type _ inline = ..
type 'a inline += X of {x : 'a}
;;
[%%expect {|
type _ inline = ..
type 'a inline += X of { x : 'a; }
|}]

let _ = X {x = 1};;
[%%expect {|
- : int inline = X {x = 1}
|}]

let must_be_polymorphic = fun x -> X {x};;
[%%expect {|
val must_be_polymorphic : 'a -> 'a inline = <fun>
|}]

let must_be_polymorphic : 'a . 'a -> 'a inline = fun x -> X {x};;
[%%expect {|
val must_be_polymorphic : 'a -> 'a inline = <fun>
|}]

(* Extensions must obey constraints *)

type 'a foo = .. constraint 'a = [> `Var ]
;;
[%%expect {|
type 'a foo = .. constraint 'a = [> `Var ]
|}]

type 'a foo += A of 'a
;;
[%%expect {|
type 'a foo += A of 'a
|}]

let a = A 9
;;
[%%expect {|
Line 1, characters 10-11:
1 | let a = A 9
              ^
Error: This expression has type int but an expression was expected of type
         [> `Var ]
|}]

type 'a foo += B : int foo
;;
[%%expect {|
Line 1, characters 19-22:
1 | type 'a foo += B : int foo
                       ^^^
Error: This type int should be an instance of type [> `Var ]
|}]

(* Signatures can make an extension private *)

type foo = ..
;;
[%%expect {|
type foo = ..
|}]

module M = struct type foo += A of int end
;;
[%%expect {|
module M : sig type foo += A of int end
|}]

let a1 = M.A 10
;;
[%%expect {|
val a1 : foo = M.A 10
|}]

module type S = sig type foo += private A of int end
;;
[%%expect {|
module type S = sig type foo += private A of int end
|}]

module M_S = (M : S)
;;
[%%expect {|
module M_S : S
|}]

let is_s x =
  match x with
    M_S.A _ -> true
  | _ -> false
;;
[%%expect {|
val is_s : foo -> bool = <fun>
|}]

let a2 = M_S.A 20
;;
[%%expect {|
Line 1, characters 9-17:
1 | let a2 = M_S.A 20
             ^^^^^^^^
Error: Cannot use private constructor A to create values of type foo
|}]

(* Signatures must respect the type of the constructor *)

type ('a, 'b) bar = ..
[%%expect {|
type ('a, 'b) bar = ..
|}]

module M : sig
  type ('a, 'b) bar += A of int
end = struct
  type ('a, 'b) bar += A of float
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) bar += A of float
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) bar += A of float end
       is not included in
         sig type ('a, 'b) bar += A of int end
       Extension declarations do not match:
         type ('a, 'b) bar += A of float
       is not included in
         type ('a, 'b) bar += A of int
       Constructors do not match:
         A of float
       is not the same as:
         A of int
       The type float is not equal to the type int
|}]

module M : sig
  type ('a, 'b) bar += A of 'a
end = struct
  type ('a, 'b) bar += A of 'b
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) bar += A of 'b
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) bar += A of 'b end
       is not included in
         sig type ('a, 'b) bar += A of 'a end
       Extension declarations do not match:
         type ('a, 'b) bar += A of 'b
       is not included in
         type ('a, 'b) bar += A of 'a
       Constructors do not match:
         A of 'b
       is not the same as:
         A of 'a
       The type 'b is not equal to the type 'a
|}]

module M : sig
  type ('a, 'b) bar = A of 'a
end = struct
  type ('b, 'a) bar = A of 'a
end;;
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('b, 'a) bar = A of 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type ('b, 'a) bar = A of 'a end
       is not included in
         sig type ('a, 'b) bar = A of 'a end
       Type declarations do not match:
         type ('b, 'a) bar = A of 'a
       is not included in
         type ('a, 'b) bar = A of 'a
       Constructors do not match:
         A of 'a
       is not the same as:
         A of 'a
       The type 'a is not equal to the type 'b
|}];;


module M : sig
  type ('a, 'b) bar += A : 'c -> ('c, 'd) bar
end = struct
  type ('a, 'b) bar += A : 'd -> ('c, 'd) bar
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) bar += A : 'd -> ('c, 'd) bar
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) bar += A : 'd -> ('c, 'd) bar end
       is not included in
         sig type ('a, 'b) bar += A : 'c -> ('c, 'd) bar end
       Extension declarations do not match:
         type ('a, 'b) bar += A : 'd -> ('c, 'd) bar
       is not included in
         type ('a, 'b) bar += A : 'c -> ('c, 'd) bar
       Constructors do not match:
         A : 'd -> ('c, 'd) bar
       is not the same as:
         A : 'c -> ('c, 'd) bar
       The type 'd is not equal to the type 'c
|}]

(* Extensions can be rebound *)

type foo = ..
;;
[%%expect {|
type foo = ..
|}]

module M = struct type foo += A1 of int end
;;
[%%expect {|
module M : sig type foo += A1 of int end
|}]

type foo += A2 = M.A1
;;
[%%expect {|
type foo += A2 of int
|}]

type bar = ..
;;
[%%expect {|
type bar = ..
|}]

type bar += A3 = M.A1
;;
[%%expect {|
Line 1, characters 17-21:
1 | type bar += A3 = M.A1
                     ^^^^
Error: The constructor M.A1 has type foo but was expected to be of type bar
|}]

module M = struct type foo += private B1 of int end
;;
[%%expect {|
module M : sig type foo += private B1 of int end
|}]

type foo += private B2 = M.B1
;;
[%%expect {|
type foo += private B2 of int
|}]

type foo += B3 = M.B1
;;
[%%expect {|
Line 1, characters 17-21:
1 | type foo += B3 = M.B1
                     ^^^^
Error: The constructor M.B1 is private
|}]

type foo += C = Unknown
;;
[%%expect {|
Line 1, characters 16-23:
1 | type foo += C = Unknown
                    ^^^^^^^
Error: Unbound constructor Unknown
|}]

(* Extensions can be rebound even if type is private *)

module M : sig type foo = private .. type foo += A1 of int end
  = struct type foo = .. type foo += A1 of int end;;
[%%expect {|
module M : sig type foo = private .. type foo += A1 of int end
|}]

type M.foo += A2 = M.A1;;
[%%expect {|
type M.foo += A2 of int
|}]

(* Rebinding handles abbreviations *)

type 'a foo = ..
;;
[%%expect {|
type 'a foo = ..
|}]

type 'a foo1 = 'a foo = ..
;;
[%%expect {|
type 'a foo1 = 'a foo = ..
|}]

type 'a foo2 = 'a foo = ..
;;
[%%expect {|
type 'a foo2 = 'a foo = ..
|}]

type 'a foo1 +=
    A of int
  | B of 'a
  | C : int foo1
;;
[%%expect {|
type 'a foo1 += A of int | B of 'a | C : int foo1
|}]

type 'a foo2 +=
    D = A
  | E = B
  | F = C
;;
[%%expect {|
type 'a foo2 += D of int | E of 'a | F : int foo2
|}]

(* Extensions must obey variances *)

type +'a foo = ..
;;
[%%expect {|
type +'a foo = ..
|}]

type 'a foo += A of (int -> 'a)
;;
[%%expect {|
type 'a foo += A of (int -> 'a)
|}]

type 'a foo += B of ('a -> int)
;;
[%%expect {|
Line 1, characters 0-31:
1 | type 'a foo += B of ('a -> int)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective contravariant.
|}]

type _ foo += C : ('a -> int) -> 'a foo
;;
[%%expect {|
Line 1, characters 0-39:
1 | type _ foo += C : ('a -> int) -> 'a foo
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective contravariant.
|}]

type 'a bar = ..
;;
[%%expect {|
type 'a bar = ..
|}]

type +'a bar += D of (int -> 'a)
;;
[%%expect {|
Line 1, characters 0-32:
1 | type +'a bar += D of (int -> 'a)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This extension does not match the definition of type bar
       Their variances do not agree.
|}]

(* Exceptions are compatible with extensions *)

module M : sig
  type exn +=
      Foo of int * float
    | Bar : 'a list -> exn
end = struct
  exception Bar : 'a list -> exn
  exception Foo of int * float
end
;;
[%%expect {|
module M : sig type exn += Foo of int * float | Bar : 'a list -> exn  end
|}]

module M : sig
  exception Bar : 'a list -> exn
  exception Foo of int * float
end = struct
  type exn +=
      Foo of int * float
    | Bar : 'a list -> exn
end
;;
[%%expect {|
module M :
  sig exception Bar : 'a list -> exn exception Foo of int * float end
|}]

exception Foo of int * float
;;
[%%expect {|
exception Foo of int * float
|}]

exception Bar : 'a list -> exn
;;
[%%expect {|
exception Bar : 'a list -> exn
|}]

module M : sig
  type exn +=
      Foo of int * float
    | Bar : 'a list -> exn
end = struct
  exception Bar = Bar
  exception Foo = Foo
end
;;
[%%expect {|
module M : sig type exn += Foo of int * float | Bar : 'a list -> exn  end
|}]

(* Test toplevel printing *)

type foo = ..
;;
[%%expect {|
type foo = ..
|}]

type foo +=
    Foo of int * int option
  | Bar of int option
;;
[%%expect {|
type foo += Foo of int * int option | Bar of int option
|}]

let x = Foo(3, Some 4), Bar(Some 5) (* Prints Foo and Bar successfully *)
;;
[%%expect {|
val x : foo * foo = (Foo (3, Some 4), Bar (Some 5))
|}]

type foo += Foo of string
;;
[%%expect {|
type foo += Foo of string
|}]

let y = x (* Prints Bar but not Foo (which has been shadowed) *)
;;
[%%expect {|
val y : foo * foo = (<extension>, Bar (Some 5))
|}]

exception Foo of int * int option
;;
[%%expect {|
exception Foo of int * int option
|}]

exception Bar of int option
;;
[%%expect {|
exception Bar of int option
|}]

let x = Foo(3, Some 4), Bar(Some 5) (* Prints Foo and Bar successfully *)
;;
[%%expect {|
val x : exn * exn = (Foo (3, Some 4), Bar (Some 5))
|}]

type foo += Foo of string
;;
[%%expect {|
type foo += Foo of string
|}]

let y = x (* Prints Bar and part of Foo (which has been shadowed) *)
;;
[%%expect {|
val y : exn * exn = (Foo (3, _), Bar (Some 5))
|}]

module Empty = struct end
module F(X:sig end) = struct
  type t = ..
  type t += A
end
let x = let open F(Empty) in (A:F(Empty).t) (* A is not printed *)
[%%expect {|
module Empty : sig end
module F : functor (X : sig end) -> sig type t = .. type t += A end
val x : F(Empty).t = <extension>
|}]


(* Test Obj functions *)

type foo = ..
;;
[%%expect {|
type foo = ..
|}]

type foo +=
    Foo
  | Bar of int
;;
[%%expect {|
type foo += Foo | Bar of int
|}]

let extension_name e = Obj.Extension_constructor.name
    (Obj.Extension_constructor.of_val e)
;;
[%%expect {|
val extension_name : 'a -> string = <fun>
|}]

let extension_id e = Obj.Extension_constructor.id
    (Obj.Extension_constructor.of_val e)
;;
[%%expect {|
val extension_id : 'a -> int = <fun>
|}]

let n1 = extension_name Foo
;;
[%%expect {|
val n1 : string = "Foo"
|}]

let n2 = extension_name (Bar 1)
;;
[%%expect {|
val n2 : string = "Bar"
|}]

let t = (extension_id (Bar 2)) = (extension_id (Bar 3))
;;
[%%expect {|
val t : bool = true
|}]

let f = (extension_id (Bar 2)) = (extension_id Foo)
;;
[%%expect {|
val f : bool = false
|}]

let is_foo x = (extension_id Foo) = (extension_id x)
;;
[%%expect {|
val is_foo : 'a -> bool = <fun>
|}]

type foo += Foo
;;
[%%expect {|
type foo += Foo
|}]

let f = is_foo Foo
;;
[%%expect {|
val f : bool = false
|}]

let _ = Obj.Extension_constructor.of_val 7
;;
[%%expect {|
Exception: Invalid_argument "Obj.extension_constructor".
|}]

let _ = Obj.Extension_constructor.of_val (object method m = 3 end)
;;
[%%expect {|
Exception: Invalid_argument "Obj.extension_constructor".
|}]
