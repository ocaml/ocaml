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
Line _, characters 9-10:
Warning 3: deprecated: t
module X : sig type t type s val x : t end
|}]

type t = X.t
;;
[%%expect{|
Line _, characters 9-12:
Warning 3: deprecated: X.t
type t = X.t
|}]

let x = X.x
;;
[%%expect{|
Line _, characters 8-11:
Warning 3: deprecated: X.x
val x : X.t = <abstr>
|}]

(* Warning control on type declaration *)

type t = X.t * X.s
;;
[%%expect{|
Line _, characters 9-12:
Warning 3: deprecated: X.t
Line _, characters 15-18:
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
Line _, characters 9-12:
Warning 3: deprecated: X.s
type t1 = X.t
and t2 = X.s
|}]

type t = A of t [@@ocaml.deprecated]
;;
[%%expect{|
Line _, characters 14-15:
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

(* Warning control on type expressions *)

type t = (X.t * X.s) [@ocaml.warning "-3"]
;;
[%%expect{|
type t = X.t * X.s
|}]

type t = (X.t [@ocaml.warning "-3"]) * X.s
;;
[%%expect{|
Line _, characters 39-42:
Warning 3: deprecated: X.s
type t = X.t * X.s
|}]


type t = A of (t [@ocaml.warning "-3"])
  [@@ocaml.deprecated]
;;
[%%expect{|
type t = A of t
|}]

(* Warning control on pattern expressions *)

let _ = function (_ : X.t) -> ()
;;
[%%expect{|
Line _, characters 22-25:
Warning 3: deprecated: X.t
- : X.t -> unit = <fun>
|}]

let _ = function (_ : X.t)[@ocaml.warning "-3"] -> ()
;;
[%%expect{|
- : X.t -> unit = <fun>
|}]


(* Warning control on module expression and module declaration *)

module M = struct let x = X.x end
;;
[%%expect{|
Line _, characters 26-29:
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
Line _, characters 26-29:
Warning 3: deprecated: X.t
Line _, characters 26-29:
Warning 3: deprecated: X.t
Line _, characters 51-54:
Warning 3: deprecated: X.x
module rec M : sig val x : X.t end
|}]

module rec M : sig val x: X.t end = struct let x = X.x end [@@ocaml.warning "-3"]
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
Line _, characters 17-20:
Warning 3: deprecated: X.x
module rec M : sig val x : X.t end
|}]

(* Warning control on module type expression and module type declaration *)

module type S = sig type t = X.t end
;;
[%%expect{|
Line _, characters 29-32:
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


(* Warning control on class expressions, class declarations and class fields *)

class c = object method x = X.x end
;;
[%%expect{|
Line _, characters 28-31:
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

(* Warning control on class type expressions, class type declarations
   and class type fields *)

class type c = object method x : X.t end
;;
[%%expect{|
Line _, characters 33-36:
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

external foo: unit -> X.t = "foo"
;;
[%%expect{|
Line _, characters 22-25:
Warning 3: deprecated: X.t
external foo : unit -> X.t = "foo"
|}]

external foo: unit -> X.t = "foo"[@@ocaml.warning "-3"]
;;
[%%expect{|
external foo : unit -> X.t = "foo"
|}]


(* open / include *)

module D = struct end[@@ocaml.deprecated]

open D
;;
[%%expect{|
module D : sig  end
Line _, characters 5-6:
Warning 3: deprecated: module D
|}]

open D [@@ocaml.warning "-3"]
;;
[%%expect{|
|}]

include D
;;
[%%expect{|
Line _, characters 8-9:
Warning 3: deprecated: module D
|}]

include D [@@ocaml.warning "-3"]
;;
[%%expect{|
|}]


(* type extension *)

type ext = ..
;;
[%%expect{|
|}]

type ext +=
  | A of X.t
  | B of (X.s [@ocaml.warning "-3"])
  | C of X.u [@ocaml.warning "-3"]
;;
[%%expect{|
|}]

type ext +=
  | C of X.t
  [@@ocaml.warning "-3"]
;;
[%%expect{|
|}]


exception Foo of X.t
;;
[%%expect{|
|}]

exception Foo of X.t [@ocaml.warning "-3"]
;;
[%%expect{|
|}]


(* Label/constructor declaration *)

type t =
  | A of X.t
  | B of X.s [@ocaml.warning "-3"]
  | C of (X.u [@ocaml.warning "-3"])
;;
[%%expect{|
|}]

type s =
  {
    a: X.t;
    b: X.s [@ocaml.warning "-3"];
    c: (X.u [@ocaml.warning "-3"]);
  }
;;
[%%expect{|
|}]
