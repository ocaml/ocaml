(* TEST
   expect;
*)

module M : sig
  class \#and : object
    val mutable \#and : int
    method \#and : int
  end
end = struct
  class \#and =
    let \#and = 1 in
    object
      val mutable \#and = \#and
      method \#and = 2
    end
end
let obj = new M.\#and
[%%expect{|
module M :
  sig class \#and : object val mutable \#and : int method \#and : int end end
val obj : M.\#and = <obj>
|}]

module M : sig type \#and = int end = struct type \#and = string end
[%%expect{|
Line 1, characters 38-68:
1 | module M : sig type \#and = int end = struct type \#and = string end
                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type \#and = string end
       is not included in
         sig type \#and = int end
       Type declarations do not match:
         type \#and = string
       is not included in
         type \#and = int
       The type "string" is not equal to the type "int"
|}]

let x = (`\#let `\#and : [ `\#let of [ `\#and ] ])
let `\#let \#rec = x
[%%expect{|
val x : [ `\#let of [ `\#and ] ] = `\#let `\#and
val \#rec : [ `\#and ] = `\#and
|}]


let f g ~\#let ?\#and ?(\#for = \#and) () =
  g ~\#let ?\#and ()
[%%expect{|
val f :
  (\#let:'a -> ?\#and:'b -> unit -> 'c) ->
  \#let:'a -> ?\#and:'b -> ?\#for:'b option -> unit -> 'c = <fun>
|}]


type t = '\#let
[%%expect{|
Line 1, characters 9-15:
1 | type t = '\#let
             ^^^^^^
Error: The type variable "'\#let" is unbound in this type declaration.
|}]

type \#mutable = { mutable \#mutable : \#mutable }
let rec \#rec = { \#mutable = \#rec }
[%%expect{|
type \#mutable = { mutable \#mutable : \#mutable; }
val \#rec : \#mutable = {\#mutable = <cycle>}
|}]

type \#and = ..
type \#and += Foo
[%%expect{|
type \#and = ..
type \#and += Foo
|}]

let x = (++);;
[%%expect{|
Line 1, characters 8-12:
1 | let x = (++);;
            ^^^^
Error: Unbound value "(++)"
|}]

let x = \#let;;
[%%expect{|
Line 1, characters 8-13:
1 | let x = \#let;;
            ^^^^^
Error: Unbound value "\#let"
|}]

let f ~\#let ?\#and () = 1
[%%expect{|
val f : \#let:'a -> ?\#and:'b -> unit -> int = <fun>
|}]

let x = (true:int)
[%%expect {|
Line 1, characters 9-13:
1 | let x = (true:int)
             ^^^^
Error: The constructor "true" has type "bool"
       but an expression was expected of type "int"
|}]

module M = struct type \#true = true end
let x = M.(true)
[%%expect {|
module M : sig type \#true = true end
val x : M.\#true = M.(true)
|}]

type t = { \#false:int; x:int }
type u = { \#true:int }

let f { \#false; \#true } = 0
[%%expect {|
type t = { \#false : int; x : int; }
type u = { \#true : int; }
Line 4, characters 17-23:
4 | let f { \#false; \#true } = 0
                     ^^^^^^
Error: The record field "\#true" belongs to the type "u"
       but is mixed here with fields of type "t"
|}]


module M = struct
  type t = { \#true:int; y:int}
  type r = { \#true:int; y:int}
end
type t = { \#false:int }
let _ = ( { M.\#true=0 } : t );;
[%%expect {|
module M :
  sig
    type t = { \#true : int; y : int; }
    type r = { \#true : int; y : int; }
  end
type t = { \#false : int; }
Line 6, characters 12-20:
6 | let _ = ( { M.\#true=0 } : t );;
                ^^^^^^^^
Error: The field "M.\#true" belongs to one of the following record types:
         "M.r"  "M.t"
       but a field was expected belonging to the record type "t"
|}]
