(* TEST
 * expect
*)

(********************************** Equality **********************************)

module M : sig
  type ('a, 'b) t = 'a * 'b
end = struct
  type ('a, 'b) t = 'a * 'a
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t = 'a * 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t = 'a * 'a end
       is not included in
         sig type ('a, 'b) t = 'a * 'b end
       Type declarations do not match:
         type ('a, 'b) t = 'a * 'a
       is not included in
         type ('a, 'b) t = 'a * 'b
       The type 'a * 'a is not equal to the type 'a * 'b
       Type 'a is not equal to type 'b
|}];;

module M : sig
  type ('a, 'b) t = 'a * 'a
end = struct
  type ('a, 'b) t = 'a * 'b
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t = 'a * 'b
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t = 'a * 'b end
       is not included in
         sig type ('a, 'b) t = 'a * 'a end
       Type declarations do not match:
         type ('a, 'b) t = 'a * 'b
       is not included in
         type ('a, 'b) t = 'a * 'a
       The type 'a * 'b is not equal to the type 'a * 'a
       Type 'b is not equal to type 'a
|}];;

type 'a x
module M: sig
  type ('a,'b,'c) t = ('a * 'b * 'c * 'b * 'a) x
end = struct
  type ('b,'c,'a) t = ('b * 'c * 'a * 'c * 'a) x
end
[%%expect{|
type 'a x
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type ('b,'c,'a) t = ('b * 'c * 'a * 'c * 'a) x
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('b, 'c, 'a) t = ('b * 'c * 'a * 'c * 'a) x end
       is not included in
         sig type ('a, 'b, 'c) t = ('a * 'b * 'c * 'b * 'a) x end
       Type declarations do not match:
         type ('b, 'c, 'a) t = ('b * 'c * 'a * 'c * 'a) x
       is not included in
         type ('a, 'b, 'c) t = ('a * 'b * 'c * 'b * 'a) x
       The type ('b * 'c * 'a * 'c * 'a) x is not equal to the type
         ('b * 'c * 'a * 'c * 'b) x
       Type 'a is not equal to type 'b
|}]

module M : sig
  type t = <m : 'b. 'b * ('b * <m:'c. 'c * 'bar> as 'bar)>
end = struct
  type t = <m : 'a. 'a * ('a * 'foo)> as 'foo
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <m : 'a. 'a * ('a * 'foo)> as 'foo
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < m : 'a. 'a * ('a * 'b) > as 'b end
       is not included in
         sig type t = < m : 'b. 'b * ('b * < m : 'c. 'c * 'a > as 'a) > end
       Type declarations do not match:
         type t = < m : 'a. 'a * ('a * 'b) > as 'b
       is not included in
         type t = < m : 'b. 'b * ('b * < m : 'c. 'c * 'a > as 'a) >
       The type < m : 'a. 'a * ('a * 'd) > as 'd is not equal to the type
         < m : 'b. 'b * ('b * < m : 'c. 'c * 'e > as 'e) >
       The method m has type 'a. 'a * ('a * < m : 'a. 'f >) as 'f,
       but the expected method type was 'c. 'c * ('b * < m : 'c. 'g >) as 'g
       The universal variable 'b would escape its scope
|}];;

type s = private < m : int; .. >;;
[%%expect{|
type s = private < m : int; .. >
|}];;

module M : sig
  type t = s
end = struct
  type t = <m : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <m : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < m : int > end
       is not included in
         sig type t = s end
       Type declarations do not match:
         type t = < m : int >
       is not included in
         type t = s
       The type < m : int > is not equal to the type s
       The second object type has an abstract row, it cannot be closed
|}];;

module M : sig
  type t = <m : int>
end = struct
  type t = s
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = s
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = s end
       is not included in
         sig type t = < m : int > end
       Type declarations do not match:
         type t = s
       is not included in
         type t = < m : int >
       The type s is not equal to the type < m : int >
       The first object type has an abstract row, it cannot be closed
|}];;

module M : sig
  type t =
    | Foo of (int)*float
end = struct
  type t =
    | Foo of (int*int)*float
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of (int*int)*float
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of (int * int) * float end
       is not included in
         sig type t = Foo of int * float end
       Type declarations do not match:
         type t = Foo of (int * int) * float
       is not included in
         type t = Foo of int * float
       Constructors do not match:
         Foo of (int * int) * float
       is not the same as:
         Foo of int * float
       The type int * int is not equal to the type int
|}];;

module M : sig
  type t = (int * float)
end = struct
  type t = (int * float * int)
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = (int * float * int)
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = int * float * int end
       is not included in
         sig type t = int * float end
       Type declarations do not match:
         type t = int * float * int
       is not included in
         type t = int * float
       The type int * float * int is not equal to the type int * float
|}];;

module M : sig
  type t = <n : int; m : float>
end = struct
  type t = <n : int; f : float>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <n : int; f : float>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < f : float; n : int > end
       is not included in
         sig type t = < m : float; n : int > end
       Type declarations do not match:
         type t = < f : float; n : int >
       is not included in
         type t = < m : float; n : int >
       The type < f : float; n : int > is not equal to the type
         < m : float; n : int >
       The second object type has no method f
|}];;

module M : sig
  type t = <n : int; m : float>
end = struct
  type t = <n : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <n : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < n : int > end
       is not included in
         sig type t = < m : float; n : int > end
       Type declarations do not match:
         type t = < n : int >
       is not included in
         type t = < m : float; n : int >
       The type < n : int > is not equal to the type < m : float; n : int >
       The first object type has no method m
|}];;

module M4 : sig
  type t = <n : int; m : float * int>
end = struct
  type t = <n : int; m : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <n : int; m : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < m : int; n : int > end
       is not included in
         sig type t = < m : float * int; n : int > end
       Type declarations do not match:
         type t = < m : int; n : int >
       is not included in
         type t = < m : float * int; n : int >
       The type < m : int; n : int > is not equal to the type
         < m : float * int; n : int >
       Types for method m are incompatible
|}];;

module M4 : sig
  type t =
    | Foo of [`Foo of string | `Bar of string]
end = struct
  type t =
    | Foo of [`Bar of string]
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of [`Bar of string]
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of [ `Bar of string ] end
       is not included in
         sig type t = Foo of [ `Bar of string | `Foo of string ] end
       Type declarations do not match:
         type t = Foo of [ `Bar of string ]
       is not included in
         type t = Foo of [ `Bar of string | `Foo of string ]
       Constructors do not match:
         Foo of [ `Bar of string ]
       is not the same as:
         Foo of [ `Bar of string | `Foo of string ]
       The type [ `Bar of string ] is not equal to the type
         [ `Bar of string | `Foo of string ]
       The first variant type does not allow tag(s) `Foo
|}];;

module M : sig
  type t = private [`C of int]
end = struct
  type t = private [`C]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`C]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `C ] end
       is not included in
         sig type t = private [ `C of int ] end
       Type declarations do not match:
         type t = private [ `C ]
       is not included in
         type t = private [ `C of int ]
       The type [ `C ] is not equal to the type [ `C of int ]
       Types for tag `C are incompatible
|}];;

module M : sig
  type t = private [`C]
end = struct
  type t = private [`C of int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`C of int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `C of int ] end
       is not included in
         sig type t = private [ `C ] end
       Type declarations do not match:
         type t = private [ `C of int ]
       is not included in
         type t = private [ `C ]
       The type [ `C of int ] is not equal to the type [ `C ]
       Types for tag `C are incompatible
|}];;

module M : sig
  type t = [`C of [< `A] | `C of [`A]]
end = struct
  type t = [`C of [< `A | `B] | `C of [`A]]
end;;
[%%expect{|
module M : sig type t = [ `C of [ `A ] ] end
|}];;

module M : sig
  type t = private [> `A of int]
end = struct
  type t = private [`A of int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`A of int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `A of int ] end
       is not included in
         sig type t = private [> `A of int ] end
       Type declarations do not match:
         type t = private [ `A of int ]
       is not included in
         type t = private [> `A of int ]
       The type [ `A of int ] is not equal to the type [> `A of int ]
       The second variant type is open and the first is not
|}];;

module M : sig
  type t = private [`A of int]
end = struct
  type t = private [> `A of int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [> `A of int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [> `A of int ] end
       is not included in
         sig type t = private [ `A of int ] end
       Type declarations do not match:
         type t = private [> `A of int ]
       is not included in
         type t = private [ `A of int ]
       The type [> `A of int ] is not equal to the type [ `A of int ]
       The first variant type is open and the second is not
|}];;

module M : sig
  type 'a t =  [> `A of int | `B of int] as 'a
end = struct
  type 'a t =  [> `A of int] as 'a
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t =  [> `A of int] as 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a constraint 'a = [> `A of int ] end
       is not included in
         sig type 'a t = 'a constraint 'a = [> `A of int | `B of int ] end
       Type declarations do not match:
         type 'a t = 'a constraint 'a = [> `A of int ]
       is not included in
         type 'a t = 'a constraint 'a = [> `A of int | `B of int ]
       The type [> `A of int ] is not equal to the type
         [> `A of int | `B of int ]
       The first variant type does not allow tag(s) `B
|}];;

module M : sig
  type 'a t =  [> `A of int] as 'a
end = struct
  type 'a t =  [> `A of int | `C of float] as 'a
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t =  [> `A of int | `C of float] as 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a constraint 'a = [> `A of int | `C of float ] end
       is not included in
         sig type 'a t = 'a constraint 'a = [> `A of int ] end
       Type declarations do not match:
         type 'a t = 'a constraint 'a = [> `A of int | `C of float ]
       is not included in
         type 'a t = 'a constraint 'a = [> `A of int ]
       The type [> `A of int | `C of float ] is not equal to the type
         [> `A of int ]
       The second variant type does not allow tag(s) `C
|}];;

module M : sig
  type t = [`C of [< `A | `B] | `C of [`A]]
end = struct
  type t = [`C of [< `A] | `C of [`A]]
end;;
[%%expect{|
module M : sig type t = [ `C of [ `A ] ] end
|}];;

module M : sig
  type t = private [< `C]
end = struct
  type t = private [< `C of int&float]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `C of int&float]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `C of int & float ] end
       is not included in
         sig type t = private [< `C ] end
       Type declarations do not match:
         type t = private [< `C of int & float ]
       is not included in
         type t = private [< `C ]
       Types for tag `C are incompatible
|}];;

(********************************** Moregen ***********************************)

module type T = sig
  type t
end
module Int = struct
  type t = int
end
module type S = sig
  module Choice : T
  val r : Choice.t list ref ref
end
module Force (X : functor () -> S) = struct end
module Choose () = struct
  module Choice =
    (val (module Int : T))
  let r = ref (ref [])
end
module Ignore = Force(Choose)
[%%expect{|
module type T = sig type t end
module Int : sig type t = int end
module type S = sig module Choice : T val r : Choice.t list ref ref end
module Force : functor (X : functor () -> S) -> sig end
module Choose :
  functor () -> sig module Choice : T val r : '_weak1 list ref ref end
Line 17, characters 16-29:
17 | module Ignore = Force(Choose)
                     ^^^^^^^^^^^^^
Error: Modules do not match:
       functor () -> sig module Choice : T val r : '_weak1 list ref ref end
     is not included in functor () -> S
     Modules do not match:
       sig module Choice : T val r : '_weak1 list ref ref end
     is not included in
       S
     Values do not match:
       val r : '_weak1 list ref ref
     is not included in
       val r : Choice.t list ref ref
     The type '_weak1 list ref ref is not compatible with the type
       Choice.t list ref ref
     The type constructor Choice.t would escape its scope
|}];;

module O = struct
  module type s
  module M: sig
    val f: (module s) -> unit
  end = struct
    module type s
    let f (module X:s) = ()
  end
end;;
[%%expect{|
Lines 5-8, characters 8-5:
5 | ........struct
6 |     module type s
7 |     let f (module X:s) = ()
8 |   end
Error: Signature mismatch:
       Modules do not match:
         sig module type s val f : (module s) -> unit end
       is not included in
         sig val f : (module s) -> unit end
       Values do not match:
         val f : (module s/1) -> unit
       is not included in
         val f : (module s/2) -> unit
       The type (module s/1) -> unit is not compatible with the type
         (module s/2) -> unit
       Type (module s/1) is not compatible with type (module s/2)
       Line 6, characters 4-17:
         Definition of module type s/1
       Line 2, characters 2-15:
         Definition of module type s/2
|}];;

module M : sig
  val f : (<m : 'b. ('b * <m: 'c. 'c * 'bar> as 'bar)>) -> unit
end = struct
  let f (x : <m : 'a. ('a * 'foo)> as 'foo) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : <m : 'a. ('a * 'foo)> as 'foo) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : (< m : 'a. 'a * 'b > as 'b) -> unit end
       is not included in
         sig val f : < m : 'b. 'b * < m : 'c. 'c * 'a > as 'a > -> unit end
       Values do not match:
         val f : (< m : 'a. 'a * 'b > as 'b) -> unit
       is not included in
         val f : < m : 'b. 'b * < m : 'c. 'c * 'a > as 'a > -> unit
       The type (< m : 'a. 'a * 'd > as 'd) -> unit
       is not compatible with the type
         < m : 'b. 'b * < m : 'c. 'c * 'e > as 'e > -> unit
       The method m has type 'a. 'a * < m : 'a. 'f > as 'f,
       but the expected method type was 'c. 'c * ('b * < m : 'c. 'g >) as 'g
       The universal variable 'b would escape its scope
|}];;

type s = private < m : int; .. >;;

module M : sig
  val f : s -> s
end = struct
  let f (x : <m : int>) = x
end;;
[%%expect{|
type s = private < m : int; .. >
Lines 5-7, characters 6-3:
5 | ......struct
6 |   let f (x : <m : int>) = x
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < m : int > -> < m : int > end
       is not included in
         sig val f : s -> s end
       Values do not match:
         val f : < m : int > -> < m : int >
       is not included in
         val f : s -> s
       The type < m : int > -> < m : int > is not compatible with the type
         s -> s
       Type < m : int > is not compatible with type s = < m : int; .. >
       The second object type has an abstract row, it cannot be closed
|}];;

module M : sig
  val f : 'a -> float
end = struct
  let f : 'b -> int = fun _ -> 0
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : 'b -> int = fun _ -> 0
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'b -> int end
       is not included in
         sig val f : 'a -> float end
       Values do not match:
         val f : 'b -> int
       is not included in
         val f : 'a -> float
       The type 'a -> int is not compatible with the type 'a -> float
       Type int is not compatible with type float
|}]

module M : sig
  val x : 'a list ref
end = struct
  let x = ref []
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x = ref []
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val x : '_weak2 list ref end
       is not included in
         sig val x : 'a list ref end
       Values do not match:
         val x : '_weak2 list ref
       is not included in
         val x : 'a list ref
       The type '_weak2 list ref is not compatible with the type 'a list ref
       Type '_weak2 is not compatible with type 'a
|}];;

module M = struct let r = ref [] end;;
type t;;
module N : sig val r : t list ref end = M;;
[%%expect{|
module M : sig val r : '_weak3 list ref end
type t
Line 3, characters 40-41:
3 | module N : sig val r : t list ref end = M;;
                                            ^
Error: Signature mismatch:
       Modules do not match:
         sig val r : '_weak3 list ref end
       is not included in
         sig val r : t list ref end
       Values do not match:
         val r : '_weak3 list ref
       is not included in
         val r : t list ref
       The type '_weak3 list ref is not compatible with the type t list ref
       The type constructor t would escape its scope
|}];;

type (_, _) eq = Refl : ('a, 'a) eq;;

module T : sig
  type t
  type s
  val eq : (t, s) eq
end = struct
  type t = int
  type s = int
  let eq = Refl
end;;

module M = struct let r = ref [] end;;

let foo p (e : (T.t, T.s) eq) (x : T.t) (y : T.s) =
  match e with
  | Refl ->
    let z = if p then x else y in
    let module N = struct
      module type S = module type of struct let r = ref [z] end
    end in
    let module O : N.S = M in
    ();;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
module T : sig type t type s val eq : (t, s) eq end
module M : sig val r : '_weak4 list ref end
Line 22, characters 25-26:
22 |     let module O : N.S = M in
                              ^
Error: Signature mismatch:
       Modules do not match:
         sig val r : '_weak4 list ref end
       is not included in
         N.S
       Values do not match:
         val r : '_weak4 list ref
       is not included in
         val r : T.t list ref
       The type '_weak4 list ref is not compatible with the type T.t list ref
       This instance of T.t is ambiguous:
       it would escape the scope of its equation
|}];;

module M: sig
  val f : int -> float
end = struct
  let f (x : 'a) = x
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : 'a) = x
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         sig val f : int -> float end
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : int -> float
       The type int -> int is not compatible with the type int -> float
       Type int is not compatible with type float
|}];;

module M: sig
  val f : (int * float * int) -> (int -> int)
end = struct
  let f (x : (int * int)) = x
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : (int * int)) = x
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : int * int -> int * int end
       is not included in
         sig val f : int * float * int -> int -> int end
       Values do not match:
         val f : int * int -> int * int
       is not included in
         val f : int * float * int -> int -> int
       The type int * int -> int * int is not compatible with the type
         int * float * int -> int -> int
       Type int * int is not compatible with type int * float * int
|}];;

module M: sig
  val f : <m : int; n : float> -> <m : int; n : float>
end = struct
  let f (x : <m : int; f : float>) = x
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : <m : int; f : float>) = x
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < f : float; m : int > -> < f : float; m : int > end
       is not included in
         sig val f : < m : int; n : float > -> < m : int; n : float > end
       Values do not match:
         val f : < f : float; m : int > -> < f : float; m : int >
       is not included in
         val f : < m : int; n : float > -> < m : int; n : float >
       The type < f : float; m : int > -> < f : float; m : int >
       is not compatible with the type
         < m : int; n : float > -> < m : int; n : float >
       The second object type has no method f
|}];;

module M : sig
  val f : [`Foo] -> unit
end = struct
  let f (x : [ `Foo | `Bar]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [ `Foo | `Bar]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [ `Bar | `Foo ] -> unit end
       is not included in
         sig val f : [ `Foo ] -> unit end
       Values do not match:
         val f : [ `Bar | `Foo ] -> unit
       is not included in
         val f : [ `Foo ] -> unit
       The type [ `Bar | `Foo ] -> unit is not compatible with the type
         [ `Foo ] -> unit
       The second variant type does not allow tag(s) `Bar
|}];;

module M : sig
  val f : [>`Foo] -> unit
end = struct
  let f (x : [< `Foo]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [< `Foo]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [< `Foo ] -> unit end
       is not included in
         sig val f : [> `Foo ] -> unit end
       Values do not match:
         val f : [< `Foo ] -> unit
       is not included in
         val f : [> `Foo ] -> unit
       The type [< `Foo ] -> unit is not compatible with the type
         [> `Foo ] -> unit
       The second variant type is open and the first is not
|}];;

module M : sig
  val f : [< `Foo | `Bar] -> unit
end = struct
  let f (x : [< `Foo]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [< `Foo]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [< `Foo ] -> unit end
       is not included in
         sig val f : [< `Bar | `Foo ] -> unit end
       Values do not match:
         val f : [< `Foo ] -> unit
       is not included in
         val f : [< `Bar | `Foo ] -> unit
       The type [< `Foo ] -> unit is not compatible with the type
         [< `Bar | `Foo ] -> unit
       The first variant type does not allow tag(s) `Bar
|}];;

module M : sig
  val f : < m : [< `Foo]> -> unit
end = struct
  let f (x : < m : 'a. [< `Foo] as 'a >) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : < m : 'a. [< `Foo] as 'a >) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < m : 'a. [< `Foo ] as 'a > -> unit end
       is not included in
         sig val f : < m : [< `Foo ] > -> unit end
       Values do not match:
         val f : < m : 'a. [< `Foo ] as 'a > -> unit
       is not included in
         val f : < m : [< `Foo ] > -> unit
       The type < m : 'a. [< `Foo ] as 'a > -> unit
       is not compatible with the type < m : [< `Foo ] > -> unit
       Types for method m are incompatible
|}];;

module M : sig
  val f : < m : 'a. [< `Foo] as 'a > -> unit
end = struct
  let f (x : < m : [`Foo]>) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : < m : [`Foo]>) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < m : [ `Foo ] > -> unit end
       is not included in
         sig val f : < m : 'a. [< `Foo ] as 'a > -> unit end
       Values do not match:
         val f : < m : [ `Foo ] > -> unit
       is not included in
         val f : < m : 'a. [< `Foo ] as 'a > -> unit
       The type < m : [ `Foo ] > -> unit is not compatible with the type
         < m : 'a. [< `Foo ] as 'a > -> unit
       Types for method m are incompatible
|}];;

module M : sig
  val f : [< `C] -> unit
end = struct
  let f (x : [< `C of int&float]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [< `C of int&float]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [< `C of int & float ] -> unit end
       is not included in
         sig val f : [< `C ] -> unit end
       Values do not match:
         val f : [< `C of int & float ] -> unit
       is not included in
         val f : [< `C ] -> unit
       The type [< `C of & int & float ] -> unit
       is not compatible with the type [< `C ] -> unit
       Types for tag `C are incompatible
|}];;

module M : sig
  val f : [`Foo] -> unit
end = struct
  let f (x : [`Foo of int]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [`Foo of int]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [ `Foo of int ] -> unit end
       is not included in
         sig val f : [ `Foo ] -> unit end
       Values do not match:
         val f : [ `Foo of int ] -> unit
       is not included in
         val f : [ `Foo ] -> unit
       The type [ `Foo of int ] -> unit is not compatible with the type
         [ `Foo ] -> unit
       Types for tag `Foo are incompatible
|}];;

module M : sig
  val f : [`Foo of int] -> unit
end = struct
  let f (x : [`Foo]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [`Foo]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [ `Foo ] -> unit end
       is not included in
         sig val f : [ `Foo of int ] -> unit end
       Values do not match:
         val f : [ `Foo ] -> unit
       is not included in
         val f : [ `Foo of int ] -> unit
       The type [ `Foo ] -> unit is not compatible with the type
         [ `Foo of int ] -> unit
       Types for tag `Foo are incompatible
|}];;

module M : sig
  val f : [< `Foo | `Bar | `Baz] -> unit
end = struct
  let f (x : [< `Foo | `Bar | `Baz]) = ()
end;;
[%%expect{|
module M : sig val f : [< `Bar | `Baz | `Foo ] -> unit end
|}];;

module M : sig
  val f : [< `Foo | `Bar | `Baz] -> unit
end = struct
  let f (x : [> `Foo | `Bar]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [> `Foo | `Bar]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [> `Bar | `Foo ] -> unit end
       is not included in
         sig val f : [< `Bar | `Baz | `Foo ] -> unit end
       Values do not match:
         val f : [> `Bar | `Foo ] -> unit
       is not included in
         val f : [< `Bar | `Baz | `Foo ] -> unit
       The type [> `Bar | `Foo ] -> unit is not compatible with the type
         [< `Bar | `Baz | `Foo ] -> unit
       The tag `Foo is guaranteed to be present in the first variant type,
       but not in the second
|}];;

(******************************* Type manifests *******************************)

module M : sig
  type t = private [< `A | `B]
end = struct
  type t = [`C]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`C]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `C ] end
       is not included in
         sig type t = private [< `A | `B ] end
       Type declarations do not match:
         type t = [ `C ]
       is not included in
         type t = private [< `A | `B ]
       The constructor C is only present in the second declaration.
|}];;

module M : sig
  type t = private [< `A | `B]
end = struct
  type t = private [> `A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [> `A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [> `A ] end
       is not included in
         sig type t = private [< `A | `B ] end
       Type declarations do not match:
         type t = private [> `A ]
       is not included in
         type t = private [< `A | `B ]
       The second is private and closed, but the first is not closed
|}];;

module M : sig
  type t = private [< `A | `B > `A]
end = struct
  type t = [`B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `B ] end
       is not included in
         sig type t = private [< `A | `B > `A ] end
       Type declarations do not match:
         type t = [ `B ]
       is not included in
         type t = private [< `A | `B > `A ]
       The constructor A is only present in the first declaration.
|}];;

module M : sig
  type t = private [> `A of int]
end = struct
  type t = [`A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `A ] end
       is not included in
         sig type t = private [> `A of int ] end
       Type declarations do not match:
         type t = [ `A ]
       is not included in
         type t = private [> `A of int ]
       Types for tag `A are incompatible
|}];;

module M : sig
   type t = private [< `A of int]
end = struct
   type t = private [< `A of & int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |    type t = private [< `A of & int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A of & int ] end
       is not included in
         sig type t = private [< `A of int ] end
       Type declarations do not match:
         type t = private [< `A of & int ]
       is not included in
         type t = private [< `A of int ]
       Types for tag `A are incompatible
|}];;


module M : sig
  type t = private [< `A of int]
end = struct
  type t = private [< `A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A ] end
       is not included in
         sig type t = private [< `A of int ] end
       Type declarations do not match:
         type t = private [< `A ]
       is not included in
         type t = private [< `A of int ]
       Types for tag `A are incompatible
|}];;


module M : sig
  type t = private [< `A of int & float]
end = struct
  type t = private [< `A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A ] end
       is not included in
         sig type t = private [< `A of int & float ] end
       Type declarations do not match:
         type t = private [< `A ]
       is not included in
         type t = private [< `A of int & float ]
       Types for tag `A are incompatible
|}];;

module M : sig
  type t = private [> `A of int]
end = struct
  type t = [`A of float]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`A of float]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `A of float ] end
       is not included in
         sig type t = private [> `A of int ] end
       Type declarations do not match:
         type t = [ `A of float ]
       is not included in
         type t = private [> `A of int ]
       The type float is not equal to the type int
|}];;

module M : sig
  type t = private [< `A | `B]
end = struct
  type t = private [`A | `B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`A | `B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `A | `B ] end
       is not included in
         sig type t = private [< `A | `B ] end
       Type declarations do not match:
         type t = private [ `A | `B ]
       is not included in
         type t = private [< `A | `B ]
       The type [ `A | `B ] is not equal to the type [< `A | `B ]
       The tag `B is guaranteed to be present in the first variant type,
       but not in the second
|}];;

module M : sig
  type t = [`A | `B]
end = struct
  type t = private [`A | `B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`A | `B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `A | `B ] end
       is not included in
         sig type t = [ `A | `B ] end
       Type declarations do not match:
         type t = private [ `A | `B ]
       is not included in
         type t = [ `A | `B ]
       A private type abbreviation would be revealed.
|}];;

module M : sig
  type t = private [< `A | `B > `B]
end = struct
  type t = private [< `A | `B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A | `B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A | `B ] end
       is not included in
         sig type t = private [< `A | `B > `B ] end
       Type declarations do not match:
         type t = private [< `A | `B ]
       is not included in
         type t = private [< `A | `B > `B ]
       The tag `B is present in the the second declaration,
       but might not be in the the first
|}];;

module M : sig
  type t = private <a : int; ..>
end = struct
  type t = <b : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <b : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < b : int > end
       is not included in
         sig type t = private < a : int; .. > end
       Type declarations do not match:
         type t = < b : int >
       is not included in
         type t = private < a : int; .. >
       The implementation is missing the method a
|}];;

module M : sig
  type t = private <a : float; ..>
end = struct
  type t = <a : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <a : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < a : int > end
       is not included in
         sig type t = private < a : float; .. > end
       Type declarations do not match:
         type t = < a : int >
       is not included in
         type t = private < a : float; .. >
       The type int is not equal to the type float
       Type int is not equal to type float
|}];;

type w = private float
type q = private (int * w)
type u = private (int * q)
module M : sig (* Confussing error message :( *)
  type t = private (int * (int * int))
end = struct
  type t = private u
end;;
[%%expect{|
type w = private float
type q = private int * w
type u = private int * q
Lines 6-8, characters 6-3:
6 | ......struct
7 |   type t = private u
8 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private u end
       is not included in
         sig type t = private int * (int * int) end
       Type declarations do not match:
         type t = private u
       is not included in
         type t = private int * (int * int)
       The type int * q is not equal to the type int * (int * int)
       Type q is not equal to type int * int
|}];;

type w = float
type q = (int * w)
type u = private (int * q)
module M : sig
  type t = private (int * (int * int))
end = struct
  type t = private u
end;;
[%%expect{|
type w = float
type q = int * w
type u = private int * q
Lines 6-8, characters 6-3:
6 | ......struct
7 |   type t = private u
8 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private u end
       is not included in
         sig type t = private int * (int * int) end
       Type declarations do not match:
         type t = private u
       is not included in
         type t = private int * (int * int)
       The type int * q is not equal to the type int * (int * int)
       Type q = int * w is not equal to type int * int
       Type w = float is not equal to type int
|}];;

type s = private int

module M : sig
  type t = private float
end = struct
  type t = private s
end;;
[%%expect{|
type s = private int
Lines 5-7, characters 6-3:
5 | ......struct
6 |   type t = private s
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private s end
       is not included in
         sig type t = private float end
       Type declarations do not match:
         type t = private s
       is not included in
         type t = private float
       The type int is not equal to the type float
|}];;

module M : sig
  type t = A
end = struct
  type t = private A
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A end
       is not included in
         sig type t = A end
       Type declarations do not match:
         type t = private A
       is not included in
         type t = A
       Private variant constructor(s) would be revealed.
|}];;

module M : sig
  type t = A | B
end = struct
  type t = private A | B
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A | B
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A | B end
       is not included in
         sig type t = A | B end
       Type declarations do not match:
         type t = private A | B
       is not included in
         type t = A | B
       Private variant constructor(s) would be revealed.
|}];;

module M : sig
  type t = A of { x : int; y : bool }
end = struct
  type t = private A of { x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A of { x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A of { x : int; y : bool; } end
       is not included in
         sig type t = A of { x : int; y : bool; } end
       Type declarations do not match:
         type t = private A of { x : int; y : bool; }
       is not included in
         type t = A of { x : int; y : bool; }
       Private variant constructor(s) would be revealed.
|}];;

module M : sig
  type t = { x : int; y : bool }
end = struct
  type t = private { x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private { x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private { x : int; y : bool; } end
       is not included in
         sig type t = { x : int; y : bool; } end
       Type declarations do not match:
         type t = private { x : int; y : bool; }
       is not included in
         type t = { x : int; y : bool; }
       A private record constructor would be revealed.
|}];;

module M : sig
  type t = A
end = struct
  type t = private A | B
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A | B
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A | B end
       is not included in
         sig type t = A end
       Type declarations do not match:
         type t = private A | B
       is not included in
         type t = A
       Private variant constructor(s) would be revealed.
|}];;

module M : sig
  type t = A | B
end = struct
  type t = private A
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A end
       is not included in
         sig type t = A | B end
       Type declarations do not match:
         type t = private A
       is not included in
         type t = A | B
       Private variant constructor(s) would be revealed.
|}];;

module M : sig
  type t = { x : int }
end = struct
  type t = private { x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private { x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private { x : int; y : bool; } end
       is not included in
         sig type t = { x : int; } end
       Type declarations do not match:
         type t = private { x : int; y : bool; }
       is not included in
         type t = { x : int; }
       A private record constructor would be revealed.
|}];;

module M : sig
  type t = { x : int; y : bool }
end = struct
  type t = private { x : int }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private { x : int }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private { x : int; } end
       is not included in
         sig type t = { x : int; y : bool; } end
       Type declarations do not match:
         type t = private { x : int; }
       is not included in
         type t = { x : int; y : bool; }
       A private record constructor would be revealed.
|}];;

module M : sig
  type t = A | B
end = struct
  type t = private { x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private { x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private { x : int; y : bool; } end
       is not included in
         sig type t = A | B end
       Type declarations do not match:
         type t = private { x : int; y : bool; }
       is not included in
         type t = A | B
       The first is a record, but the second is a variant.
|}];;

module M : sig
  type t = { x : int; y : bool }
end = struct
  type t = private A | B
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A | B
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A | B end
       is not included in
         sig type t = { x : int; y : bool; } end
       Type declarations do not match:
         type t = private A | B
       is not included in
         type t = { x : int; y : bool; }
       The first is a variant, but the second is a record.
|}];;

module M : sig
  type t = [`A]
end = struct
  type t = private [> `A | `B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [> `A | `B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [> `A | `B ] end
       is not included in
         sig type t = [ `A ] end
       Type declarations do not match:
         type t = private [> `A | `B ]
       is not included in
         type t = [ `A ]
       A private row type would be revealed.
|}];;

module M : sig
  type t = [`A]
end = struct
  type t = private [< `A | `B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A | `B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A | `B ] end
       is not included in
         sig type t = [ `A ] end
       Type declarations do not match:
         type t = private [< `A | `B ]
       is not included in
         type t = [ `A ]
       A private row type would be revealed.
|}];;

module M : sig
  type t = [`A]
end = struct
  type t = private [< `A | `B > `A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A | `B > `A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A | `B > `A ] end
       is not included in
         sig type t = [ `A ] end
       Type declarations do not match:
         type t = private [< `A | `B > `A ]
       is not included in
         type t = [ `A ]
       A private row type would be revealed.
|}];;

module M : sig
  type t = < m : int >
end = struct
  type t = private < m : int; .. >
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private < m : int; .. >
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private < m : int; .. > end
       is not included in
         sig type t = < m : int > end
       Type declarations do not match:
         type t = private < m : int; .. >
       is not included in
         type t = < m : int >
       A private row type would be revealed.
|}];;
