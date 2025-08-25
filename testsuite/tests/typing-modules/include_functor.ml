(* TEST
  flags = "-w +a";
  set OCAMLRUNPARAM="b";
  expect;
*)

(* Test 1: Basic usage in structs *)
module type S = sig
  type t
  val x : t
end

module F1 (X : S) = struct
  let y = X.x
end

module M1 = struct
  type t = int
  let x = 5

  include functor F1
end

let () = assert Int.(equal M1.y 5);;
[%%expect{|
module type S = sig type t val x : t end
module F1 : (X : S) -> sig val y : X.t end
module M1 : sig type t = int val x : int val y : int end
|}];;

(* Test 2: Wrong type in structure *)
module M2 = struct
  type t = int
  let x = true

  include functor F1
end;;
[%%expect{|
Line 5, characters 18-20:
5 |   include functor F1
                      ^^
Error: Signature mismatch in included functor's parameter:
       Values do not match: val x : bool is not included in val x : t
       The type "bool" is not compatible with the type "t" = "int"
|}];;

(* Test 3: Missing type in structure *)
module M3 = struct
  let x = 5

  include functor F1
end;;
[%%expect{|
Line 4, characters 18-20:
4 |   include functor F1
                      ^^
Error: Signature mismatch in included functor's parameter:
       The type "t" is required but not provided
|}];;

(* Test 4: Missing value in structure *)
module M4 = struct
  type t = int
  let y = 5

  include functor F1
end;;
[%%expect{|
Line 5, characters 18-20:
5 |   include functor F1
                      ^^
Error: Signature mismatch in included functor's parameter:
       The value "x" is required but not provided
|}];;

(* Test 5: Include functor in signature *)
module type T = sig
  type s
  val f : s -> bool
end

module type F5 = functor (X : S) -> T with type s = X.t

module type M5_sig = sig
  type t
  val x : t

  include T with type s = t
end

module M5_impl : M5_sig = struct
  type t = int
  type s = t

  let x = 5
  let f s = x = s
end
let () = assert (M5_impl.f M5_impl.x);;
[%%expect{|
module type T = sig type s val f : s -> bool end
module type F5 = (X : S) -> sig type s = X.t val f : s -> bool end
module type M5_sig = sig type t val x : t type s = t val f : s -> bool end
module M5_impl : M5_sig
|}];;

(* Test 6: Nested module names work *)
module type Eq9 = sig
  type t
  val z : t
  val equal : t -> t -> bool
end

module type S9 = sig
  module Foo : Eq9
end

module F9 (X : S9) = struct
  let eq_z = X.Foo.equal X.Foo.z
end

module Int9 : sig
  type t
  val equal : t -> t -> bool
  val of_int : int -> t
end = struct
  include Int
  let of_int t = t
end

module M9 = struct
  module Foo : Eq9 = struct
    include Int9
    let z = of_int 7
  end
  include functor F9
end

let () = assert (M9.eq_z M9.Foo.z);;
[%%expect{|
module type Eq9 = sig type t val z : t val equal : t -> t -> bool end
module type S9 = sig module Foo : Eq9 end
module F9 : (X : S9) -> sig val eq_z : X.Foo.t -> bool end
module Int9 : sig type t val equal : t -> t -> bool val of_int : int -> t end
module M9 : sig module Foo : Eq9 val eq_z : Foo.t -> bool end
|}];;

let () = assert (M9.eq_z 7);;
[%%expect{|
Line 1, characters 25-26:
1 | let () = assert (M9.eq_z 7);;
                             ^
Error: The constant "7" has type "int" but an expression was expected of type
         "M9.Foo.t"
|}];;

module M9' = struct
  module Foo = struct
    include Int9
    let z = of_int 6
  end
  include functor F9
end

let () = assert (not (M9'.eq_z (M9'.Foo.of_int 5)))
let () = assert (M9'.eq_z (M9'.Foo.of_int 6));;
[%%expect{|
module M9' :
  sig
    module Foo :
      sig
        type t = Int9.t
        val equal : t -> t -> bool
        val of_int : int -> t
        val z : t
      end
    val eq_z : Int9.t -> bool
  end
|}];;

(* Test 7: nondep_supertype: Get good error if we need a name for the
   parameter. *)
module F10 (X : Set.OrderedType) = struct
  let s : Set.Make(X).t = assert false
end

module M10 = struct
  type t = T
  let compare _ _ = 0
  include functor F10
end;;
[%%expect{|
module F10 : (X : Set.OrderedType) -> sig val s : Set.Make(X).t end
Line 8, characters 18-21:
8 |   include functor F10
                      ^^^
Error: This functor has type
       "(X : Set.OrderedType) -> sig val s : Set.Make(X).t end"
       The parameter cannot be eliminated in the result type.
       This functor can't be included directly; please apply it to an explicit argument.
|}];;

(* Test 8: Include functor should work at the toplevel (and check shadowing). *)
type t = int
let x : t = 3
let x : t = 5
include functor F1

let () = assert (Int.(equal y 5));;
[%%expect{|
type t = int
val x : t = 3
val x : t = 5
val y : int = 5
|}];;

type t = int
let x : t = 5
let x : t = 3
include functor F1

let () = assert (Int.(equal y 5));;
[%%expect{|
type t = int
val x : t = 5
val x : t = 3
val y : int = 3
Exception: Assert_failure ("", 6, 9).
|}]

(* Test 9: Check that things get marked used appropriately when they are
   used by include functor.  (And that we're getting the warnings we expect
   to see if they weren't). *)
module M12_1 : sig val y : int list end = struct
  module Bar = struct
    type t = int
    let x = 5
  end

  module F (G :
    sig
      module T_sub : sig type t val x : t end
                  -> sig type t val x : t end
    end) = struct
    module Foo = G.T_sub(Bar)
    let y = Foo.x
  end

  module T_sub (X : sig type t val x : t end) = struct
    type t = X.t list
    let x = [X.x]
  end
  include functor F
end;;
[%%expect{|
module M12_1 : sig val y : int list end
|}];;

module M12_2 : sig val y : int list end = struct
  module Bar = struct
    type t = int
    let x = 5
    let q = 42
  end

  module F (G :
    sig
      module T_sub : sig type t val x : t end
                  -> sig type t val x : t end
    end) = struct
    module Foo = G.T_sub(Bar)
    let y = Foo.x
  end

  module T_sub (X : sig type t val x : t end) = struct
    type t = X.t list
    let x = [X.x]
    let z = "something"
  end
  include functor F
end;;
[%%expect{|
Line 5, characters 8-9:
5 |     let q = 42
            ^
Warning 32 [unused-value-declaration]: unused value "q".

Line 20, characters 8-9:
20 |     let z = "something"
             ^
Warning 32 [unused-value-declaration]: unused value "z".

module M12_2 : sig val y : int list end
|}];;

module M12_3 : sig val y : int list end = struct
  module Bar = struct
    type t = int
    let x = 5
  end

  module F (G :
    sig
      module T_sub : sig type t val x : t end
                  -> sig type t val x : t end
    end) = struct
    module Foo = G.T_sub(Bar)
    let y = Foo.x
  end

  module T_sub (X : sig type t val x : t end) = struct
    type t = X.t list
    let x = [X.x]
  end

  let y = [Bar.x]
end;;

[%%expect{|
Line 9, characters 32-41:
9 |       module T_sub : sig type t val x : t end
                                    ^^^^^^^^^
Warning 32 [unused-value-declaration]: unused value "x".

Line 13, characters 8-9:
13 |     let y = Foo.x
             ^
Warning 32 [unused-value-declaration]: unused value "y".

Lines 7-14, characters 2-5:
 7 | ..module F (G :
 8 |     sig
 9 |       module T_sub : sig type t val x : t end
10 |                   -> sig type t val x : t end
11 |     end) = struct
12 |     module Foo = G.T_sub(Bar)
13 |     let y = Foo.x
14 |   end
Warning 60 [unused-module]: unused module "F".

Line 17, characters 4-21:
17 |     type t = X.t list
         ^^^^^^^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type "t".

Line 18, characters 8-9:
18 |     let x = [X.x]
             ^
Warning 32 [unused-value-declaration]: unused value "x".

Lines 16-19, characters 2-5:
16 | ..module T_sub (X : sig type t val x : t end) = struct
17 |     type t = X.t list
18 |     let x = [X.x]
19 |   end
Warning 60 [unused-module]: unused module "T_sub".

module M12_3 : sig val y : int list end
|}]


(* Test 10: Check that we reject including a functor with multiple arguments *)
module F14 (X : S) (Y : S) = struct
  let z = (X.x, Y.x)
end

module M14 = struct
  type t = int
  let x : t = 5

  include functor F14
end;;
[%%expect{|
module F14 : (X : S) (Y : S) -> sig val z : X.t * Y.t end
Line 9, characters 18-21:
9 |   include functor F14
                      ^^^
Error: The type of this functor's result is not includable; it is
       (Y : S) -> sig val z : X.t * Y.t end
|}];;

module F14_2 (X : S) () () = struct
  let z = X.x
end

module M14_2 = struct
  type t = int
  let x : t = 5

  include functor F14_2
end;;
[%%expect{|
module F14_2 : (X : S) () () -> sig val z : X.t end
Line 9, characters 18-23:
9 |   include functor F14_2
                      ^^^^^
Error: The type of this functor's result is not includable; it is
       () () -> sig val z : X.t end
|}];;

(* Test 11: Make sure we're extracting functor return types appropriately *)
module type S15 = sig val x : int end
module type S15' = S15

module F15 (X : sig end) : S15' =
struct
  let x = 42
end

include functor F15

[%%expect{|
module type S15 = sig val x : int end
module type S15' = S15
Line 4, characters 12-13:
4 | module F15 (X : sig end) : S15' =
                ^
Warning 60 [unused-module]: unused module "X".

module F15 : (X : sig end) -> S15'
val x : int = 42
|}]

(* Test 12: Functors whose types don't begin with a normal applicative parameter
   are rejected. *)
module type S17 = sig
  type t
  val x : t
end

module F17_1 : () -> S17 = functor () -> struct
  type t = int
  let x = Random.int 42
end

module G17 = struct
  include functor F17_1
end;;
[%%expect {|
module type S17 = sig type t val x : t end
module F17_1 : () -> S17
Line 12, characters 18-23:
12 |   include functor F17_1
                       ^^^^^
Error: The type of this functor is: () -> S17.
       Its parameter is not a signature.
|}];;

module F17_2 = functor () (X : S17) -> struct
  let z : X.t = X.x
end

module G17_2 = struct
  type t = T
  let x : t = T
  include functor F17_2
end;;
[%%expect {|
module F17_2 : () (X : S17) -> sig val z : X.t end
Line 8, characters 18-23:
8 |   include functor F17_2
                      ^^^^^
Error: The type of this functor is: () (X : S17) -> sig val z : X.t end.
       Its parameter is not a signature.
|}];;

(* Test 13: Generative functors *)
module type S18 = sig
  type t
  val x : t
  val equal : t -> t -> bool
end

module F18 (X : S18) () : sig
  type t'
  val z : t'
  val equal_t' : t' -> t' -> bool
end = struct
  type t' = X.t
  let z = X.x
  let equal_t' = X.equal
end

module M18 = struct
  type t = int
  let x = 42
  let equal = Int.equal
  include functor F18
end

let () = assert (M18.equal_t' M18.z M18.z)
[%%expect{|
module type S18 = sig type t val x : t val equal : t -> t -> bool end
module F18 :
  (X : S18) () -> sig type t' val z : t' val equal_t' : t' -> t' -> bool end
module M18 :
  sig
    type t = int
    val x : int
    val equal : int -> int -> bool
    type t'
    val z : t'
    val equal_t' : t' -> t' -> bool
  end
|}];;

module F18_2 (X : S18) () : sig
  type t'
  val z : t'
end = struct
  type t' = X.t
  let z = X.x
end

module M18_2 (Y : S18) = struct
  include Y
  include functor F18_2
end;;
[%%expect{|
module F18_2 : (X : S18) () -> sig type t' val z : t' end
Line 11, characters 18-23:
11 |   include functor F18_2
                       ^^^^^
Error: This functor creates fresh types when applied.
       Including it is not allowed inside applicative functors.
|}];;

(* Test 14: Effects happen when they should *)
let r19 = ref 0

module F19 (X : sig val x : int end) = struct
  let () = r19 := X.x
end

let () = assert (Int.equal 0 !r19)

module M19 = struct
  let x = 42
  let () = assert (Int.equal 0 !r19)

  include functor F19

  let () = assert (Int.equal 42 !r19)
end

let () = assert (Int.equal 42 !r19);;
[%%expect{|
val r19 : int ref = {contents = 0}
module F19 : (X : sig val x : int end) -> sig end
module M19 : sig val x : int end
|}];;

(* Test 15: Shadowed types *)
module I20 = struct
  type t = int
end

module F20 (M : sig
    type t = string
  end) =
struct
  let go (arg : M.t) = print_endline arg
end

module M20 = struct
  include I20

  type t = string

  include functor F20
end

let () = M20.go 3;;
[%%expect{|
module I20 : sig type t = int end
module F20 : (M : sig type t = string end) -> sig val go : M.t -> unit end
module M20 : sig type t = string val go : string -> unit end
Line 20, characters 16-17:
20 | let () = M20.go 3;;
                     ^
Error: The constant "3" has type "int" but an expression was expected of type
         "string"
|}];;

(* Test 16: Check that scraping of result type happens in environment expanded
   with parameter type. *)
module M21 = struct
  module F (_ : sig end) = struct
    module type S = sig end
  end

  module P = struct
    module Make (M : sig end) : F(M).S = struct end
  end

  include functor P.Make
end;;
[%%expect{|
module M21 :
  sig
    module F : sig end -> sig module type S = sig end end
    module P : sig module Make : (M : sig end) -> F(M).S end
  end
|}];;

