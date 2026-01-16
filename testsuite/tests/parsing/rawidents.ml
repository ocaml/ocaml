(* TEST
   flags="-dsource";
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
  sig class \#and : object val  mutable \#and : int method  \#and : int end
  end =
  struct
    class \#and = let \#and = 1 in
      object val mutable \#and = \#and method \#and = 2 end
  end ;;
module M :
  sig class \#and : object val mutable \#and : int method \#and : int end end

let obj = new M.\#and;;
val obj : M.\#and = <obj>
|}]

module M : sig type \#and = int end = struct type \#and = string end
[%%expect{|

module M : sig type \#and = int end = struct type \#and = string end ;;
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

let x = (`\#let `\#and : [ `\#let of [ `\#and ] ]);;
val x : [ `\#let of [ `\#and ] ] = `\#let `\#and

let `\#let \#rec = x;;
val \#rec : [ `\#and ] = `\#and
|}]


let f g ~\#let ?\#and ?(\#for = \#and) () =
  g ~\#let ?\#and ()
[%%expect{|

let f g ~\#let ?\#and ?(\#for= \#and) () = g ~\#let ?\#and ();;
val f :
  (\#let:'a -> ?\#and:'b -> unit -> 'c) ->
  \#let:'a -> ?\#and:'b -> ?\#for:'b option -> unit -> 'c = <fun>
|}]


type t = '\#let
[%%expect{|

type t = '\#let;;
Line 1, characters 9-15:
1 | type t = '\#let
             ^^^^^^
Error: The type variable "'\#let" is unbound in this type declaration.
|}]

type \#mutable = { mutable \#mutable : \#mutable }
let rec \#rec = { \#mutable = \#rec }
[%%expect{|

type \#mutable = {
  mutable \#mutable: \#mutable };;
type \#mutable = { mutable \#mutable : \#mutable; }

let rec \#rec = { \#mutable = \#rec };;
val \#rec : \#mutable = {\#mutable = <cycle>}
|}]

type \#and = ..
type \#and += Foo
[%%expect{|

type \#and = ..;;
type \#and = ..

type \#and +=
  | Foo ;;
type \#and += Foo
|}]

let x = (++);;
[%%expect{|

let x = (++);;
Line 1, characters 8-12:
1 | let x = (++);;
            ^^^^
Error: Unbound value "(++)"
|}]

let x = \#let;;
[%%expect{|

let x = \#let;;
Line 1, characters 8-13:
1 | let x = \#let;;
            ^^^^^
Error: Unbound value "\#let"
|}]

let f ~\#let ?\#and () = 1
[%%expect{|

let f ~\#let ?\#and () = 1;;
val f : \#let:'a -> ?\#and:'b -> unit -> int = <fun>
|}]

let x = (true:int)
[%%expect {|

let x = (true : int);;
Line 1, characters 9-13:
1 | let x = (true:int)
             ^^^^
Error: This expression should not be a boolean literal, the expected type is
       "int"
|}]

module M = struct type \#true = true end
let x = M.(true)
[%%expect {|

module M = struct type \#true =
                    | true  end;;
module M : sig type \#true = true end

let x = let open M in true;;
val x : M.\#true = M.(true)
|}]

type t = { \#false:int; x:int }
type u = { \#true:int }

let f { \#false; \#true } = 0
[%%expect {|

type t = {
  \#false: int ;
  x: int };;
type t = { \#false : int; x : int; }

type u = {
  \#true: int };;
type u = { \#true : int; }

let f { \#false; \#true } = 0;;
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

module M =
  struct type t = {
           \#true: int ;
           y: int }
         type r = {
           \#true: int ;
           y: int } end;;
module M :
  sig
    type t = { \#true : int; y : int; }
    type r = { \#true : int; y : int; }
  end

type t = {
  \#false: int };;
type t = { \#false : int; }

let _ = ({ M.\#true = 0 } : t);;
Line 6, characters 12-20:
6 | let _ = ( { M.\#true=0 } : t );;
                ^^^^^^^^
Error: The field "M.\#true" belongs to one of the following record types:
         "M.r"  "M.t"
       but a field was expected belonging to the record type "t"
|}]

let f (mod) = (mod)
type 'a t = { \#mod: 'a}
let f {\#mod} = (mod)#\#mod;;
[%%expect {|

let f (mod) = (mod);;
val f : 'a -> 'a = <fun>

type 'a t = {
  \#mod: 'a };;
type 'a t = { \#mod : 'a; }

let f { \#mod } = (mod)#\#mod;;
val f : < \#mod : 'a; .. > t -> 'a = <fun>
|}]

class \#mod = object
  val mutable \#mod = (mod)
  method \#mod =
    \#mod <- (mod);
    {<\#mod = (+) >}
end;;
[%%expect {|

class \#mod =
  object
    val mutable \#mod = (mod)
    method \#mod = \#mod <- (mod); {<\#mod = (+)>}
  end;;
class \#mod :
  object ('a) val mutable \#mod : int -> int -> int method \#mod : 'a end
|}]
