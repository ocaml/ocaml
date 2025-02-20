(* TEST
 expect;
*)

type t = A of {x:int; mutable y:int}
[%%expect{|
type t = A of { x : int; mutable y : int; }
|}]

let f (A r) = r  (* -> escape *)
[%%expect{|
Line 1, characters 14-15:
1 | let f (A r) = r  (* -> escape *)
                  ^
Error: The value "r" has type "t.A" but an expression was expected of type "'a"
       This instance of "t.A" is ambiguous:
       it would escape the scope of its equation
|}]

let f (A r) = r.x (* ok *)
[%%expect{|
val f : t -> int = <fun>
|}]

let f x = A {x; y = x} (* ok *)
[%%expect{|
val f : int -> t = <fun>
|}]

let f (A r) = A {r with y = r.x + 1} (* ok *)
[%%expect{|
val f : t -> t = <fun>
|}]

let f (A r) = A r (* ok *)
[%%expect{|
val f : t -> t = <fun>
|}]


let f () = A {a = 1} (* customized error message *)
[%%expect{|
Line 1, characters 14-15:
1 | let f () = A {a = 1} (* customized error message *)
                  ^
Error: The field "a" is not part of the record argument for the "t.A" constructor
|}]

let f () = A {x = 1; y = 3} (* ok *)
[%%expect{|
val f : unit -> t = <fun>
|}]

type _ t = A: {x : 'a; y : 'b} -> 'a t
[%%expect{|
type _ t = A : { x : 'a; y : 'b; } -> 'a t
|}]
let f (A {x; y}) = A {x; y = ()}  (* ok *)
[%%expect{|
val f : 'a t -> 'a t = <fun>
|}]
let f (A ({x; y} as r)) = A {x = r.x; y = r.y} (* ok *)
[%%expect{|
val f : 'a t -> 'a t = <fun>
|}]


(* other escape cases *)
let glob = ref None
let f (A r) = (glob := Some r)
[%%expect{|
val glob : '_weak1 option ref = {contents = None}
Line 2, characters 28-29:
2 | let f (A r) = (glob := Some r)
                                ^
Error: The value "r" has type "('a, $b) t.A"
       but an expression was expected of type "'weak1"
       This instance of "('a, $b) t.A" is ambiguous:
       it would escape the scope of its equation
       Hint: "$b" is an existential type bound by the constructor "A".
|}]

(* this one could arguably be accepted,
   as the record type does not leak outside the right-hand-side. *)
let f (A r) = ignore r
[%%expect{|
val f : 'a t -> unit = <fun>
|}]

(* check that scope-escape rules still work correctly through rebinding,
   even on parametrized types, even when their parameters are constrained. *)
type 'a t = A of {x: 'a; mutable y: unit} constraint 'a = int * 'b
module Alias = struct
  type 'a t2 = 'a t = A of {x: 'a; mutable y: unit}
end
let f (Alias.A r) = ignore r
let f (Alias.A {x; y}) = A {x; y = ()}
[%%expect{|
type 'a t = A of { x : 'a; mutable y : unit; } constraint 'a = int * 'b
module Alias :
  sig
    type 'a t2 = 'a t = A of { x : 'a; mutable y : unit; }
      constraint 'a = int * 'b
  end
val f : (int * 'a) Alias.t2 -> unit = <fun>
val f : (int * 'a) Alias.t2 -> (int * 'a) t = <fun>
|}]

(* record-update tests from Jacques Garrigue *)
type t = A of { x : int; y : int }
let f1 (A r) = A {r with y = 3}
[%%expect{|
type t = A of { x : int; y : int; }
val f1 : t -> t = <fun>
|}]

let f2 (A r) = let r' = {r with y = 3} in ()
[%%expect{|
Line 1, characters 19-21:
1 | let f2 (A r) = let r' = {r with y = 3} in ()
                       ^^
Warning 26 [unused-var]: unused variable "r'".

val f2 : t -> unit = <fun>
|}]

let f3 (A r) = let r' = {r with y = 1 + 2} in A r'
[%%expect{|
val f3 : t -> t = <fun>
|}]

let f4 (A r) = {r with y = 3} (* should be rejected *)
[%%expect{|
val f4 : t -> t.A = <fun>
|}]


module M = struct
  type 'a t =
    | A of {x : 'a}
    | B: {u : 'b} -> unit t

  exception Foo of {x : int}
end
[%%expect{|
module M :
  sig
    type 'a t = A of { x : 'a; } | B : { u : 'b; } -> unit t
    exception Foo of { x : int; }
  end
|}]

module N : sig
  type 'b t = 'b M.t =
    | A of {x : 'b}
    | B: {u : 'bla} -> unit t

  exception Foo of {x : int}
end = struct
  type 'b t = 'b M.t =
    | A of {x : 'b}
    | B: {u : 'z} -> unit t

  exception Foo = M.Foo
end
[%%expect{|
module N :
  sig
    type 'b t = 'b M.t = A of { x : 'b; } | B : { u : 'bla; } -> unit t
    exception Foo of { x : int; }
  end
|}]

module type S = sig exception A of {x:int}  end
module F (X : sig val x : (module S) end) = struct
  module A = (val X.x)
end  (* -> this expression creates fresh types (not really!) *)
[%%expect{|
module type S = sig exception A of { x : int; } end
Line 3, characters 13-22:
3 |   module A = (val X.x)
                 ^^^^^^^^^
Error: This expression creates fresh types.
       It is not allowed inside applicative functors.
|}]

module type S = sig
  exception A of {x : int}
  exception A of {x : string}
end
[%%expect{|
Line 3, characters 2-29:
3 |   exception A of {x : string}
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Multiple definition of the extension constructor name "A".
       Names must be unique in a given structure or signature.
|}]

module M = struct
  exception A of {x : int}
  exception A of {x : string}
end
[%%expect{|
Line 3, characters 2-29:
3 |   exception A of {x : string}
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Multiple definition of the extension constructor name "A".
       Names must be unique in a given structure or signature.
|}]

module M1 = struct
  exception A of {x : int}
end
module M = struct
  include M1
  include M1
end
[%%expect{|
module M1 : sig exception A of { x : int; } end
module M : sig exception A of { x : int; } end
|}]

module type S1 = sig
  exception A of {x : int}
end
module type S = sig
  include S1
  include S1
end
[%%expect{|
module type S1 = sig exception A of { x : int; } end
module type S = sig exception A of { x : int; } end
|}]

module M = struct
  exception A = M1.A
end
[%%expect{|
module M : sig exception A of { x : int; } end
|}]

module X1 = struct
  type t = ..
end
module X2 = struct
  type t = ..
end
module Z = struct
  type X1.t += A of {x: int}
  type X2.t += A of {x: int}
end
[%%expect{|
module X1 : sig type t = .. end
module X2 : sig type t = .. end
Line 9, characters 15-28:
9 |   type X2.t += A of {x: int}
                   ^^^^^^^^^^^^^
Error: Multiple definition of the extension constructor name "A".
       Names must be unique in a given structure or signature.
|}]

(* PR#6716 *)

type _ c = C : [`A] c
type t = T : {x:[<`A] c} -> t
let f (T { x = C }) = ()
[%%expect{|
type _ c = C : [ `A ] c
type t = T : { x : [< `A ] c; } -> t
val f : t -> unit = <fun>
|}]


(* An advanced scope-escape example from typing/types.ml *)
module Types = struct
  type any = [`some | `none | `var]
  and field_kind = [`some|`var] field_kind_gen
  and _ field_kind_gen =
      FKvar : {mutable field_kind: any field_kind_gen} -> [> `var] field_kind_gen
    | FKprivate : [> `none] field_kind_gen  (* private method; only under FKvar *)
    | FKpublic  : [> `some] field_kind_gen  (* public method *)
    | FKabsent  : [> `some] field_kind_gen  (* hidden private method *)

  let rec field_kind_internal_repr : field_kind -> field_kind = function
    | FKvar {field_kind = FKvar _ | FKpublic | FKabsent as fk} ->
        field_kind_internal_repr fk
    | kind -> kind
end
[%%expect{|
module Types :
  sig
    type any = [ `none | `some | `var ]
    and field_kind = [ `some | `var ] field_kind_gen
    and _ field_kind_gen =
        FKvar : { mutable field_kind : any field_kind_gen;
        } -> [> `var ] field_kind_gen
      | FKprivate : [> `none ] field_kind_gen
      | FKpublic : [> `some ] field_kind_gen
      | FKabsent : [> `some ] field_kind_gen
    val field_kind_internal_repr : field_kind -> field_kind
  end
|}]
