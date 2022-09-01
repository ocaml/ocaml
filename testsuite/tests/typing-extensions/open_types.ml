(* TEST
   * expect
*)

type foo = ..
;;
[%%expect {|
type foo = ..
|}]

(* Check that abbreviations work *)

type bar = foo = ..
;;
[%%expect {|
type bar = foo = ..
|}]

type baz = foo = ..
;;
[%%expect {|
type baz = foo = ..
|}]

type bar += Bar1 of int
;;
[%%expect {|
type bar += Bar1 of int
|}]

type baz += Bar2 of int
;;
[%%expect {|
type baz += Bar2 of int
|}]

module M = struct type bar += Foo of float end
;;
[%%expect {|
module M : sig type bar += Foo of float end
|}]

module type S = sig type baz += Foo of float end
;;
[%%expect {|
module type S = sig type baz += Foo of float end
|}]

module M_S = (M : S)
;;
[%%expect {|
module M_S : S
|}]

(* Abbreviations need to be made open *)

type foo = ..
;;
[%%expect {|
type foo = ..
|}]

type bar = foo
;;
[%%expect {|
type bar = foo
|}]

type bar += Bar of int
;;
[%%expect {|
Line 1, characters 0-22:
1 | type bar += Bar of int
    ^^^^^^^^^^^^^^^^^^^^^^
Error: Type definition bar is not extensible
|}]

type baz = bar = ..
;;
[%%expect {|
Line 1, characters 0-19:
1 | type baz = bar = ..
    ^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type bar
       The original is abstract, but this is an extensible variant.
|}]

(* Abbreviations need to match parameters *)

type 'a foo = ..
;;
[%%expect {|
type 'a foo = ..
|}]

type ('a, 'b) bar = 'a foo = ..
;;
[%%expect {|
Line 1, characters 0-31:
1 | type ('a, 'b) bar = 'a foo = ..
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type 'a foo
       They have different arities.
|}]

type ('a, 'b) foo = ..
;;
[%%expect {|
type ('a, 'b) foo = ..
|}]

type ('a, 'b) bar = ('a, 'a) foo = ..
;;
[%%expect {|
Line 1, characters 0-37:
1 | type ('a, 'b) bar = ('a, 'a) foo = ..
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         ('a, 'a) foo
       Their parameters differ
       The type 'a is not equal to the type 'b
|}]

(* Check that signatures can hide exstensibility *)

module M = struct type foo = .. end
;;
[%%expect {|
module M : sig type foo = .. end
|}]

module type S = sig type foo end
;;
[%%expect {|
module type S = sig type foo end
|}]

module M_S = (M : S)
;;
[%%expect {|
module M_S : S
|}]

type M_S.foo += Foo
;;
[%%expect {|
Line 1, characters 0-19:
1 | type M_S.foo += Foo
    ^^^^^^^^^^^^^^^^^^^
Error: Type definition M_S.foo is not extensible
|}]

(* Check that signatures cannot add extensibility *)

module M = struct type foo end
;;
[%%expect {|
module M : sig type foo end
|}]

module type S = sig type foo = .. end
;;
[%%expect {|
module type S = sig type foo = .. end
|}]

module M_S = (M : S)
;;
[%%expect {|
Line 1, characters 14-15:
1 | module M_S = (M : S)
                  ^
Error: Signature mismatch:
       Modules do not match: sig type foo = M.foo end is not included in S
       Type declarations do not match:
         type foo = M.foo
       is not included in
         type foo = ..
       The first is abstract, but the second is an extensible variant.
|}]

(* Check that signatures can make exstensibility private *)

module M = struct type foo = .. end
;;
[%%expect {|
module M : sig type foo = .. end
|}]

module type S = sig type foo = private .. end
;;
[%%expect {|
module type S = sig type foo = private .. end
|}]

module M_S = (M : S)
;;
[%%expect {|
module M_S : S
|}]

type M_S.foo += Foo
;;
[%%expect {|
Line 1, characters 16-19:
1 | type M_S.foo += Foo
                    ^^^
Error: Cannot extend private type definition M_S.foo
|}]

(* Check that signatures cannot make private extensibility public *)

module M = struct type foo = private .. end
;;
[%%expect {|
module M : sig type foo = private .. end
|}]

module type S = sig type foo = .. end
;;
[%%expect {|
module type S = sig type foo = .. end
|}]

module M_S = (M : S)
;;
[%%expect {|
Line 1, characters 14-15:
1 | module M_S = (M : S)
                  ^
Error: Signature mismatch:
       Modules do not match:
         sig type foo = M.foo = private .. end
       is not included in
         S
       Type declarations do not match:
         type foo = M.foo = private ..
       is not included in
         type foo = ..
       A private extensible variant would be revealed.
|}]


(* Check that signatures maintain variances *)

module M = struct type +'a foo = .. type 'a bar = 'a foo = .. end
;;
[%%expect {|
module M : sig type +'a foo = .. type 'a bar = 'a foo = .. end
|}]

module type S = sig type 'a foo = .. type 'a bar = 'a foo = .. end
;;
[%%expect {|
module type S = sig type 'a foo = .. type 'a bar = 'a foo = .. end
|}]

module M_S = (M : S)
;;
[%%expect {|
Line 1, characters 14-15:
1 | module M_S = (M : S)
                  ^
Error: Signature mismatch:
       Modules do not match:
         sig type 'a foo = 'a M.foo = .. type 'a bar = 'a foo = .. end
       is not included in
         S
       Type declarations do not match:
         type 'a foo = 'a M.foo = ..
       is not included in
         type 'a foo = ..
       Their variances do not agree.
|}]

(* Exn is an open type *)

type exn2 = exn = ..
;;
[%%expect {|
type exn2 = exn = ..
|}]

(* PR#8579 exceptions can be private *)

type exn += private Foobar
let _ = raise Foobar
;;
[%%expect {|
type exn += private Foobar
Line 2, characters 14-20:
2 | let _ = raise Foobar
                  ^^^^^^
Error: Cannot use private constructor Foobar to create values of type exn
|}]


(* Exhaustiveness *)

type foo = ..
type foo += Foo
let f = function Foo -> ()
;;
[%%expect {|
type foo = ..
type foo += Foo
Line 3, characters 8-26:
3 | let f = function Foo -> ()
            ^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
*extension*
Matching over values of extensible variant types (the *extension* above)
must include a wild card pattern in order to be exhaustive.
val f : foo -> unit = <fun>
|}]

(* More complex exhaustiveness *)

let f = function
  | [Foo] -> 1
  | _::_::_ -> 3
  | [] -> 2
;;
[%%expect {|
Lines 1-4, characters 8-11:
1 | ........function
2 |   | [Foo] -> 1
3 |   | _::_::_ -> 3
4 |   | [] -> 2
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
*extension*::[]
Matching over values of extensible variant types (the *extension* above)
must include a wild card pattern in order to be exhaustive.
val f : foo list -> int = <fun>
|}]


(* PR#7330: exhaustiveness with GADTs *)

type t = ..
type t += IPair : (int * int) -> t ;;
[%%expect {|
type t = ..
type t += IPair : (int * int) -> t
|}]

let f = function IPair (i, j) -> Format.sprintf "(%d, %d)" i j ;;
[%%expect {|
Line 1, characters 8-62:
1 | let f = function IPair (i, j) -> Format.sprintf "(%d, %d)" i j ;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
*extension*
Matching over values of extensible variant types (the *extension* above)
must include a wild card pattern in order to be exhaustive.
val f : t -> string = <fun>
|}]
