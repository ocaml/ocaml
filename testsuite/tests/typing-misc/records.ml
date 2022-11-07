(* TEST
   * expect
*)

(* undefined labels *)
type t = {x:int;y:int};;
{x=3;z=2};;
[%%expect{|
type t = { x : int; y : int; }
Line 2, characters 5-6:
2 | {x=3;z=2};;
         ^
Error: Unbound record field z
|}];;
fun {x=3;z=2} -> ();;
[%%expect{|
Line 1, characters 9-10:
1 | fun {x=3;z=2} -> ();;
             ^
Error: Unbound record field z
|}];;

(* mixed labels *)
{x=3; contents=2};;
[%%expect{|
Line 1, characters 6-14:
1 | {x=3; contents=2};;
          ^^^^^^^^
Error: The record field contents belongs to the type 'a ref
       but is mixed here with fields of type t
|}];;

(* private types *)
type u = private {mutable u:int};;
{u=3};;
[%%expect{|
type u = private { mutable u : int; }
Line 2, characters 0-5:
2 | {u=3};;
    ^^^^^
Error: Cannot create values of the private type u
|}];;
fun x -> x.u <- 3;;
[%%expect{|
Line 1, characters 11-12:
1 | fun x -> x.u <- 3;;
               ^
Error: Cannot assign field u of the private type u
|}];;

(* Punning and abbreviations *)
module M = struct
  type t = {x: int; y: int}
end;;
[%%expect{|
module M : sig type t = { x : int; y : int; } end
|}];;

let f {M.x; y} = x+y;;
let r = {M.x=1; y=2};;
let z = f r;;
[%%expect{|
val f : M.t -> int = <fun>
val r : M.t = {M.x = 1; y = 2}
val z : int = 3
|}];;

(* messages *)
type foo = { mutable y:int };;
let f (r: int) = r.y <- 3;;
[%%expect{|
type foo = { mutable y : int; }
Line 2, characters 17-18:
2 | let f (r: int) = r.y <- 3;;
                     ^
Error: This expression has type int but an expression was expected of type
         foo
|}];;

let f (r: int) =
  match r with
  | { contents = 3 } -> ()
[%%expect{|
Line 3, characters 4-20:
3 |   | { contents = 3 } -> ()
        ^^^^^^^^^^^^^^^^
Error: This pattern matches values of type int ref
       but a pattern was expected which matches values of type int
|}];;



(* bugs *)
type foo = { y: int; z: int };;
type bar = { x: int };;
let f (r: bar) = ({ r with z = 3 } : foo)
[%%expect{|
type foo = { y : int; z : int; }
type bar = { x : int; }
Line 3, characters 20-21:
3 | let f (r: bar) = ({ r with z = 3 } : foo)
                        ^
Error: This expression has type bar but an expression was expected of type
         foo
|}];;

type foo = { x: int };;
let r : foo = { ZZZ.x = 2 };;
[%%expect{|
type foo = { x : int; }
Line 2, characters 16-21:
2 | let r : foo = { ZZZ.x = 2 };;
                    ^^^^^
Error: Unbound module ZZZ
|}];;

(ZZZ.X : int option);;
[%%expect{|
Line 1, characters 1-6:
1 | (ZZZ.X : int option);;
     ^^^^^
Error: Unbound module ZZZ
|}];;

(* PR#5865 *)
let f (x : Complex.t) = x.Complex.z;;
[%%expect{|
Line 1, characters 26-35:
1 | let f (x : Complex.t) = x.Complex.z;;
                              ^^^^^^^^^
Error: Unbound record field Complex.z
|}];;

(* PR#6608 *)
{ true with contents = 0 };;
[%%expect{|
Line 1, characters 2-6:
1 | { true with contents = 0 };;
      ^^^^
Error: This expression has type bool which is not a record type.
|}];;

type ('a, 'b) t = { fst : 'a; snd : 'b };;
let with_fst r fst = { r with fst };;
with_fst { fst=""; snd="" } 2;;
[%%expect{|
type ('a, 'b) t = { fst : 'a; snd : 'b; }
val with_fst : ('a, 'b) t -> 'c -> ('c, 'b) t = <fun>
- : (int, string) t = {fst = 2; snd = ""}
|}];;

(* PR#7695 *)
type 'a t = { f : 'a; g : 'a };;
let x = { f = 12; g = 43 };;
{x with f = "hola"};;
[%%expect{|
type 'a t = { f : 'a; g : 'a; }
val x : int t = {f = 12; g = 43}
Line 3, characters 0-19:
3 | {x with f = "hola"};;
    ^^^^^^^^^^^^^^^^^^^
Error: This expression has type string t
       but an expression was expected of type int t
       Type string is not compatible with type int
|}]

(* PR#7696 *)
let r = { (assert false) with contents = 1 } ;;
[%%expect{|
Line 1, characters 8-44:
1 | let r = { (assert false) with contents = 1 } ;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 23 [useless-record-with]: all the fields are explicitly listed in this record:
the 'with' clause is useless.
Exception: Assert_failure ("", 1, 10).
|}]

(* reexport *)

type ('a,'b) def = { x:int } constraint 'b = [> `A]

type arity = (int, [`A]) def = {x:int};;
[%%expect{|
type ('a, 'b) def = { x : int; } constraint 'b = [> `A ]
Line 3, characters 0-38:
3 | type arity = (int, [`A]) def = {x:int};;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         (int, [ `A ]) def
       They have different arities.
|}]

type ('a,'b) ct = (int,'b) def = {x:int};;
[%%expect{|
Line 1, characters 0-40:
1 | type ('a,'b) ct = (int,'b) def = {x:int};;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         (int, [> `A ]) def
       Their parameters differ
       The type int is not equal to the type 'a
|}]

type ('a,'b) kind = ('a, 'b) def = A constraint 'b = [> `A];;
[%%expect{|
Line 1, characters 0-59:
1 | type ('a,'b) kind = ('a, 'b) def = A constraint 'b = [> `A];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         ('a, [> `A ]) def
       The original is a record, but this is a variant.
|}]

type d = { x:int; y : int }
type mut = d = {x:int; mutable y:int}
[%%expect{|
type d = { x : int; y : int; }
Line 2, characters 0-37:
2 | type mut = d = {x:int; mutable y:int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       Fields do not match:
         y : int;
       is not the same as:
         mutable y : int;
       This is mutable and the original is not.
|}]

type missing = d = { x:int }
[%%expect{|
Line 1, characters 0-28:
1 | type missing = d = { x:int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       An extra field, y, is provided in the original definition.
|}]

type wrong_type = d = {x:float}
[%%expect{|
Line 1, characters 0-31:
1 | type wrong_type = d = {x:float}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       1. Fields do not match:
         x : int;
       is not the same as:
         x : float;
       The type int is not equal to the type float
       2. An extra field, y, is provided in the original definition.
|}]

type mono = {foo:int}
type unboxed = mono = {foo:int} [@@unboxed]
[%%expect{|
type mono = { foo : int; }
Line 2, characters 0-43:
2 | type unboxed = mono = {foo:int} [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type mono
       Their internal representations differ:
       this definition uses unboxed representation.
|}]

type perm = d = {y:int; x:int}
[%%expect{|
Line 1, characters 0-30:
1 | type perm = d = {y:int; x:int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       Fields x and y have been swapped.
|}]

type t = { f1 : int ; f2 : int }

let f () = { f1 = 0
        ; Coq__10.f2 = 0 }

[%%expect{|
type t = { f1 : int; f2 : int; }
Line 4, characters 10-20:
4 |         ; Coq__10.f2 = 0 }
              ^^^^^^^^^^
Error: Unbound module Coq__10
|}]

module Coq__11 = struct
  type t = { f1 : int ; f2 : int; f3 : int }
end

let f () = { f1 = 0
           ; Coq__10.f2 = 0
           ; Coq__11.f3 = 0 }

[%%expect{|
module Coq__11 : sig type t = { f1 : int; f2 : int; f3 : int; } end
Line 6, characters 13-23:
6 |            ; Coq__10.f2 = 0
                 ^^^^^^^^^^
Error: Unbound module Coq__10
Hint: Did you mean Coq__11?
|}]
