(* TEST
   * expect
*)

[@@@ocaml.warning "+3"];;

module X: sig
  type t [@@ocaml.deprecated]
  type s [@@ocaml.deprecated]
  type u [@@ocaml.deprecated]
  val x: t [@@ocaml.deprecated]
end = struct
  type t = int
  type s
  type u
  let x = 0
end;;
[%%expect{|
Line 7, characters 9-10:
    val x: t [@@ocaml.deprecated]
           ^
Warning 3: deprecated: t
module X : sig type t type s type u val x : t end
|}]

type t = X.t
;;
[%%expect{|
Line 1, characters 9-12:
  type t = X.t
           ^^^
Warning 3: deprecated: X.t
type t = X.t
|}]

let x = X.x
;;
[%%expect{|
Line 1, characters 8-11:
  let x = X.x
          ^^^
Warning 3: deprecated: X.x
val x : X.t = <abstr>
|}]

(* Patterns *)

let (_, foo [@deprecated], _) = 1, (), 3
;;
foo;;
[%%expect{|
val foo : unit = ()
Line 3, characters 0-3:
  foo;;
  ^^^
Warning 3: deprecated: foo
- : unit = ()
|}]

let (_, foo, bar) [@deprecated] = 1, (), 3
;;
foo;;
[%%expect{|
val foo : unit = ()
val bar : int = 3
- : unit = ()
|}]

let f = function
  | bar, cho [@deprecated], _ -> cho + 1
;;
[%%expect{|
Line 2, characters 33-36:
    | bar, cho [@deprecated], _ -> cho + 1
                                   ^^^
Warning 3: deprecated: cho
val f : 'a * int * 'b -> int = <fun>
|}]

class c (_, (foo [@deprecated] : int)) =
  object
    val h = foo
  end
;;
[%%expect{|
Line 3, characters 12-15:
      val h = foo
              ^^^
Warning 3: deprecated: foo
class c : 'a * int -> object val h : int end
|}]

(* Type declarations *)

type t = X.t * X.s
;;
[%%expect{|
Line 1, characters 9-12:
  type t = X.t * X.s
           ^^^
Warning 3: deprecated: X.t
Line 1, characters 15-18:
  type t = X.t * X.s
                 ^^^
Warning 3: deprecated: X.s
type t = X.t * X.s
|}]

type t = X.t * X.s [@@ocaml.warning "-3"]
;;
[%%expect{|
type t = X.t * X.s
|}]

type t1 = X.t [@@ocaml.warning "-3"]
and t2 = X.s
;;
[%%expect{|
Line 2, characters 9-12:
  and t2 = X.s
           ^^^
Warning 3: deprecated: X.s
type t1 = X.t
and t2 = X.s
|}]

type t = A of t [@@ocaml.deprecated]
;;
[%%expect{|
Line 1, characters 14-15:
  type t = A of t [@@ocaml.deprecated]
                ^
Warning 3: deprecated: t
type t = A of t
|}]

type t = A of t
  [@@ocaml.deprecated]
  [@@ocaml.warning "-3"]
;;
[%%expect{|
type t = A of t
|}]

(* Type expressions *)

type t = (X.t * X.s) [@ocaml.warning "-3"]
;;
[%%expect{|
type t = X.t * X.s
|}]

type t = (X.t [@ocaml.warning "-3"]) * X.s
;;
[%%expect{|
Line 1, characters 39-42:
  type t = (X.t [@ocaml.warning "-3"]) * X.s
                                         ^^^
Warning 3: deprecated: X.s
type t = X.t * X.s
|}]


type t = A of (t [@ocaml.warning "-3"])
  [@@ocaml.deprecated]
;;
[%%expect{|
type t = A of t
|}]

(* Pattern expressions *)

let _ = function (_ : X.t) -> ()
;;
[%%expect{|
Line 1, characters 22-25:
  let _ = function (_ : X.t) -> ()
                        ^^^
Warning 3: deprecated: X.t
- : X.t -> unit = <fun>
|}]

let _ = function (_ : X.t)[@ocaml.warning "-3"] -> ()
;;
[%%expect{|
- : X.t -> unit = <fun>
|}]


(* Module expressions and module declarations *)

module M = struct let x = X.x end
;;
[%%expect{|
Line 1, characters 26-29:
  module M = struct let x = X.x end
                            ^^^
Warning 3: deprecated: X.x
module M : sig val x : X.t end
|}]

module M = (struct let x = X.x end)[@ocaml.warning "-3"]
;;
[%%expect{|
module M : sig val x : X.t end
|}]

module M = struct let x = X.x end [@@ocaml.warning "-3"]
;;
[%%expect{|
module M : sig val x : X.t end
|}]


module rec M : sig val x: X.t end = struct let x = X.x end
[%%expect{|
Line 1, characters 26-29:
  module rec M : sig val x: X.t end = struct let x = X.x end
                            ^^^
Warning 3: deprecated: X.t
Line 1, characters 51-54:
  module rec M : sig val x: X.t end = struct let x = X.x end
                                                     ^^^
Warning 3: deprecated: X.x
module rec M : sig val x : X.t end
|}]

module rec M : sig val x: X.t end =
  struct
    let x = X.x
  end [@@ocaml.warning "-3"]
[%%expect{|
module rec M : sig val x : X.t end
|}]

module rec M :
  (sig val x: X.t end)[@ocaml.warning "-3"] =
  (struct let x = X.x end)[@ocaml.warning "-3"]
[%%expect{|
module rec M : sig val x : X.t end
|}]

module rec M :
  (sig val x: X.t end)[@ocaml.warning "-3"] =
  struct let x = X.x end
[%%expect{|
Line 3, characters 17-20:
    struct let x = X.x end
                   ^^^
Warning 3: deprecated: X.x
module rec M : sig val x : X.t end
|}]

(* Module type expressions and module type declarations *)

module type S = sig type t = X.t end
;;
[%%expect{|
Line 1, characters 29-32:
  module type S = sig type t = X.t end
                               ^^^
Warning 3: deprecated: X.t
module type S = sig type t = X.t end
|}]

module type S = (sig type t = X.t end)[@ocaml.warning "-3"]
;;
[%%expect{|
module type S = sig type t = X.t end
|}]

module type S = sig type t = X.t end[@@ocaml.warning "-3"]
;;
[%%expect{|
module type S = sig type t = X.t end
|}]


(* Class expressions, class declarations and class fields *)

class c = object method x = X.x end
;;
[%%expect{|
Line 1, characters 28-31:
  class c = object method x = X.x end
                              ^^^
Warning 3: deprecated: X.x
class c : object method x : X.t end
|}]

class c = object method x = X.x end[@@ocaml.warning "-3"]
;;
[%%expect{|
class c : object method x : X.t end
|}]

class c = (object method x = X.x end)[@ocaml.warning "-3"]
;;
[%%expect{|
class c : object method x : X.t end
|}]

class c = object method x = X.x [@@ocaml.warning "-3"] end
;;
[%%expect{|
class c : object method x : X.t end
|}]

(* Class type expressions, class type declarations
   and class type fields *)

class type c = object method x : X.t end
;;
[%%expect{|
Line 1, characters 33-36:
  class type c = object method x : X.t end
                                   ^^^
Warning 3: deprecated: X.t
class type c = object method x : X.t end
|}]

class type c = object method x : X.t end[@@ocaml.warning "-3"]
;;
[%%expect{|
class type c = object method x : X.t end
|}]

class type c = object method x : X.t end[@ocaml.warning "-3"]
;;
[%%expect{|
class type c = object method x : X.t end
|}]

class type c = object method x : X.t [@@ocaml.warning "-3"] end
;;
[%%expect{|
class type c = object method x : X.t end
|}]



(* External declarations *)

external foo: unit -> X.t = "%identity"
;;
[%%expect{|
Line 1, characters 22-25:
  external foo: unit -> X.t = "%identity"
                        ^^^
Warning 3: deprecated: X.t
external foo : unit -> X.t = "%identity"
|}]

external foo: unit -> X.t = "%identity"[@@ocaml.warning "-3"]
;;
[%%expect{|
external foo : unit -> X.t = "%identity"
|}]


(* Eval *)
;;
X.x
;;
[%%expect{|
Line 1, characters 0-3:
  X.x
  ^^^
Warning 3: deprecated: X.x
- : X.t = <abstr>
|}]

;;
X.x [@@ocaml.warning "-3"]
;;
[%%expect{|
- : X.t = <abstr>
|}]

(* Open / include *)

module D = struct end[@@ocaml.deprecated]

open D
;;
[%%expect{|
module D : sig  end
Line 3, characters 5-6:
  open D
       ^
Warning 3: deprecated: module D
|}]

open D [@@ocaml.warning "-3"]
;;
[%%expect{|
|}]

include D
;;
[%%expect{|
Line 1, characters 8-9:
  include D
          ^
Warning 3: deprecated: module D
|}]

include D [@@ocaml.warning "-3"]
;;
[%%expect{|
|}]


(* Type extensions *)

type ext = ..
;;
[%%expect{|
type ext = ..
|}]

type ext +=
  | A of X.t
  | B of (X.s [@ocaml.warning "-3"])
  | C of X.u [@ocaml.warning "-3"]
;;
[%%expect{|
Line 2, characters 9-12:
    | A of X.t
           ^^^
Warning 3: deprecated: X.t
type ext += A of X.t | B of X.s | C of X.u
|}]

type ext +=
  | C of X.t
  [@@ocaml.warning "-3"]
;;
[%%expect{|
type ext += C of X.t
|}]


exception Foo of X.t
;;
[%%expect{|
Line 1, characters 17-20:
  exception Foo of X.t
                   ^^^
Warning 3: deprecated: X.t
exception Foo of X.t
|}]

exception Foo of X.t [@ocaml.warning "-3"]
;;
[%%expect{|
exception Foo of X.t
|}]


(* Labels/constructors/fields *)

type t =
  | A of X.t
  | B of X.s [@ocaml.warning "-3"]
  | C of (X.u [@ocaml.warning "-3"])
;;
[%%expect{|
Line 2, characters 9-12:
    | A of X.t
           ^^^
Warning 3: deprecated: X.t
type t = A of X.t | B of X.s | C of X.u
|}]

type t =
  {
    a: X.t;
    b: X.s [@ocaml.warning "-3"];
    c: (X.u [@ocaml.warning "-3"]);
  }
;;
[%%expect{|
Line 3, characters 7-10:
      a: X.t;
         ^^^
Warning 3: deprecated: X.t
type t = { a : X.t; b : X.s; c : X.u; }
|}]


type t =
  <
    a: X.t;
    b: X.s [@ocaml.warning "-3"];
    c: (X.u [@ocaml.warning "-3"]);
  >
;;
[%%expect{|
Line 3, characters 7-10:
      a: X.t;
         ^^^
Warning 3: deprecated: X.t
type t = < a : X.t; b : X.s; c : X.u >
|}]


type t =
  [
  | `A of X.t
  | `B of X.s [@ocaml.warning "-3"]
  | `C of (X.u [@ocaml.warning "-3"])
  ]
;;
[%%expect{|
Line 3, characters 10-13:
    | `A of X.t
            ^^^
Warning 3: deprecated: X.t
type t = [ `A of X.t | `B of X.s | `C of X.u ]
|}]


(* Test for ocaml.ppwarning, and its interactions with ocaml.warning *)


[@@@ocaml.ppwarning "Pp warning!"]
;;
[%%expect{|
Line 1, characters 20-33:
  [@@@ocaml.ppwarning "Pp warning!"]
                      ^^^^^^^^^^^^^
Warning 22: Pp warning!
|}]


let x = () [@ocaml.ppwarning "Pp warning 1!"]
    [@@ocaml.ppwarning  "Pp warning 2!"]
;;
[%%expect{|
Line 2, characters 24-39:
      [@@ocaml.ppwarning  "Pp warning 2!"]
                          ^^^^^^^^^^^^^^^
Warning 22: Pp warning 2!
Line 1, characters 29-44:
  let x = () [@ocaml.ppwarning "Pp warning 1!"]
                               ^^^^^^^^^^^^^^^
Warning 22: Pp warning 1!
val x : unit = ()
|}]

type t = unit
    [@ocaml.ppwarning "Pp warning!"]
;;
[%%expect{|
Line 2, characters 22-35:
      [@ocaml.ppwarning "Pp warning!"]
                        ^^^^^^^^^^^^^
Warning 22: Pp warning!
type t = unit
|}]

module X = struct
  [@@@ocaml.warning "-22"]

  [@@@ocaml.ppwarning "Pp warning1!"]

  [@@@ocaml.warning "+22"]

  [@@@ocaml.ppwarning "Pp warning2!"]
end
;;
[%%expect{|
Line 8, characters 22-36:
    [@@@ocaml.ppwarning "Pp warning2!"]
                        ^^^^^^^^^^^^^^
Warning 22: Pp warning2!
module X : sig  end
|}]

let x =
  ((() [@ocaml.ppwarning "Pp warning 1!"]) [@ocaml.warning "-22"])
    [@ocaml.ppwarning  "Pp warning 2!"]
;;
[%%expect{|
Line 3, characters 23-38:
      [@ocaml.ppwarning  "Pp warning 2!"]
                         ^^^^^^^^^^^^^^^
Warning 22: Pp warning 2!
val x : unit = ()
|}]

type t =
  ((unit [@ocaml.ppwarning "Pp warning 1!"]) [@ocaml.warning "-22"])
  [@ocaml.ppwarning  "Pp warning 2!"]
  [@@ocaml.ppwarning "Pp warning 3!"]
;;
[%%expect{|
Line 4, characters 21-36:
    [@@ocaml.ppwarning "Pp warning 3!"]
                       ^^^^^^^^^^^^^^^
Warning 22: Pp warning 3!
Line 3, characters 21-36:
    [@ocaml.ppwarning  "Pp warning 2!"]
                       ^^^^^^^^^^^^^^^
Warning 22: Pp warning 2!
type t = unit
|}]

let ([][@ocaml.ppwarning "XX"]) = []
;;
[%%expect{|
Line 1, characters 25-29:
  let ([][@ocaml.ppwarning "XX"]) = []
                           ^^^^
Warning 22: XX
Line 1, characters 4-31:
  let ([][@ocaml.ppwarning "XX"]) = []
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
_::_
|}]
let[@ocaml.warning "-8-22"] ([][@ocaml.ppwarning "XX"]) = []
;;
[%%expect{|
|}]
