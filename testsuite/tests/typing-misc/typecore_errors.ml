(* TEST
   * expect
*)


(** Gives an example for every [raise(Error(_,_,_)] in typing/typecore.ml
    that is no covered by another test in the testsuite and does not
    require special flags or ppx.
*)

(** Illegal interval *)

let x = function 0. .. 1. -> ()
[%%expect {|
Line 8, characters 17-25:
8 | let x = function 0. .. 1. -> ()
                     ^^^^^^^^
Error: Only character intervals are supported in patterns.
|}]

(** Constructor arity mismatch *)
let f = function None None -> 0

[%%expect{|
Line 1, characters 17-26:
1 | let f = function None None -> 0
                     ^^^^^^^^^
Error: The constructor None expects 0 argument(s),
       but is applied here to 1 argument(s)
|}]

let x = None None
[%%expect{|
Line 1, characters 8-17:
1 | let x = None None
            ^^^^^^^^^
Error: The constructor None expects 0 argument(s),
       but is applied here to 1 argument(s)
|}]

(** Inline record escape *)
type t = A of {x:int}
let f = function (A (x:_)) -> 0

[%%expect{|
type t = A of { x : int; }
Line 2, characters 20-25:
2 | let f = function (A (x:_)) -> 0
                        ^^^^^
Error: This form is not allowed as the type of the inlined record could escape.
|}]


(** Exception below toplevel *)
let f = function Some(exception Not_found) -> 0
[%%expect{|
Line 1, characters 21-42:
1 | let f = function Some(exception Not_found) -> 0
                         ^^^^^^^^^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]

(** Extension *)
let f = function [%ext] -> 0
[%%expect{|
Line 1, characters 19-22:
1 | let f = function [%ext] -> 0
                       ^^^
Error: Uninterpreted extension 'ext'.
|}]


(** Unification error in type approx *)

let rec f x = ( (), () : _ -> _ -> _ )
[%%expect{|
Line 3, characters 14-38:
3 | let rec f x = ( (), () : _ -> _ -> _ )
                  ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type 'a * 'b
       but an expression was expected of type 'c -> 'd -> 'e
|}]

let rec g x = ( ((), ()) : _ -> _ :> _ )
[%%expect{|
Line 1, characters 14-40:
1 | let rec g x = ( ((), ()) : _ -> _ :> _ )
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type 'a * 'b
       but an expression was expected of type 'c -> 'd
|}]


(** No value clause *)

let f x = match x with exception Not_found -> ();;
[%%expect{|
Line 3, characters 10-48:
3 | let f x = match x with exception Not_found -> ();;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: None of the patterns in this 'match' expression match values.
|}]

(** Check duplicate *)
type r = { x : int }
let r = { x= 1; x= 1}

[%%expect{|
type r = { x : int; }
Line 2, characters 8-21:
2 | let r = { x= 1; x= 1}
            ^^^^^^^^^^^^^
Error: The record field label x is defined several times
|}]

(** Non-mutable is non mutable *)
let () = { x = 1 }.x <- 2

[%%expect{|
Line 1, characters 9-25:
1 | let () = { x = 1 }.x <- 2
             ^^^^^^^^^^^^^^^^
Error: The record field x is not mutable
|}]


(** Invalid for loop *)

let () = for Some i = 3 to 4 do () done;
[%%expect{|
Line 3, characters 13-19:
3 | let () = for Some i = 3 to 4 do () done;
                 ^^^^^^
Error: Invalid for-loop index: only variables and _ are allowed.
|}]


(** Inherited methods not defined *)

class virtual v = object method virtual m: int end;;
class c = object(self)
  inherit v as super
  method m = 0
  method x: int = super#m
end;;

[%%expect{|
class virtual v : object method virtual m : int end
Line 7, characters 18-23:
7 |   method x: int = super#m
                      ^^^^^
Error: This expression has no method m
|}]

(** New virtual class *)

let x = new v

[%%expect{|
Line 3, characters 8-13:
3 | let x = new v
            ^^^^^
Error: Cannot instantiate the virtual class v
|}]


(* Immutable instance variable cannot be mutated *)
let x = object val x = 1 method m = x<-0 end

[%%expect{|
Line 1, characters 36-40:
1 | let x = object val x = 1 method m = x<-0 end
                                        ^^^^
Error: The instance variable x is not mutable
|}]

(** Self variables cannot be mutated *)
let x = object(self) method m = self <-0 end

[%%expect{|
Line 1, characters 32-40:
1 | let x = object(self) method m = self <-0 end
                                    ^^^^^^^^
Error: The value self is not an instance variable
|}]

(** Multiply override *)

class c = object val x = 0 method m: c = {< x=0; x=1 >} end

[%%expect{|
Line 3, characters 41-55:
3 | class c = object val x = 0 method m: c = {< x=0; x=1 >} end
                                             ^^^^^^^^^^^^^^
Error: The instance variable x is overridden several times
|}]

(** Override outside of classes *)

let f x = {< y = x >}

[%%expect{|
Line 3, characters 10-21:
3 | let f x = {< y = x >}
              ^^^^^^^^^^^
Error: This object duplication occurs outside a method definition
|}]


(** Unbound instance variable in object duplication *)

class c = object val x = 0 method m: c = {< y=1 >} end

[%%expect{|
Line 3, characters 41-50:
3 | class c = object val x = 0 method m: c = {< y=1 >} end
                                             ^^^^^^^^^
Error: Unbound instance variable y
|}]


(** Not a packed type *)
module type empty = sig  end
let f (x:int) = ()
let x = f (module struct end)
[%%expect {|
module type empty = sig end
val f : int -> unit = <fun>
Line 3, characters 10-29:
3 | let x = f (module struct end)
              ^^^^^^^^^^^^^^^^^^^
Error: This expression is packed module, but the expected type is int
|}]


(** Builtin [%extension_constructor *)
type t = A
let x = [%extension_constructor A]
[%%expect {|
type t = A
Line 2, characters 32-33:
2 | let x = [%extension_constructor A]
                                    ^
Error: This constructor is not an extension constructor.
|}]

let x = [%extension_constructor]
[%%expect {|
Line 1, characters 8-32:
1 | let x = [%extension_constructor]
            ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid [%extension_constructor] payload, a constructor is expected.
|}]

(** Invalid format *)
let x = format_of_string "%z"
[%%expect {|
Line 1, characters 25-29:
1 | let x = format_of_string "%z"
                             ^^^^
Error: invalid format "%z": at character number 1, invalid conversion "%z"
|}]

(** Apply wrong label *)

let f ~x = x + 2
let y = f ~y:1
[%%expect {|
val f : x:int -> int = <fun>
Line 4, characters 13-14:
4 | let y = f ~y:1
                 ^
Error: The function applied to this argument has type x:int -> int
This argument cannot be applied with label ~y
|}]

let g f = f ~x:0 ~y:0; f ~y:0 ~x:0
[%%expect {|
Line 1, characters 23-24:
1 | let g f = f ~x:0 ~y:0; f ~y:0 ~x:0
                           ^
Error: This function is applied to arguments
       in an order different from other calls.
       This is only allowed when the real type is known.
|}]

(** Inlined record *)
type t = A of { x: int }
let x = A 1
[%%expect {|
type t = A of { x : int; }
Line 2, characters 8-11:
2 | let x = A 1
            ^^^
Error: This constructor expects an inlined record argument.
|}]

(** Illegal let rec *)
type 'a t = A of 'a
let rec A x = A (A ())

[%%expect {|
type 'a t = A of 'a
Line 2, characters 8-11:
2 | let rec A x = A (A ())
            ^^^
Error: Only variables are allowed as left-hand side of `let rec'
|}]

(** Non-linear pattern *)

let quadratic (x,x) = x * x
[%%expect {|
Line 3, characters 17-18:
3 | let quadratic (x,x) = x * x
                     ^
Error: Variable x is bound several times in this matching
|}]

(** Or-patter clash *)
type t = A of int | B of float|C
let f (A x|B x) = 0
[%%expect {|
type t = A of int | B of float | C
Line 2, characters 6-15:
2 | let f (A x|B x) = 0
          ^^^^^^^^^
Error: The variable x on the left-hand side of this or-pattern has type
       int but on the right-hand side it has type float
|}]

(** Orphan pattern variable *)

let f (A x|C) = 0
[%%expect {|
Line 3, characters 6-13:
3 | let f (A x|C) = 0
          ^^^^^^^
Error: Variable x must occur on both sides of this | pattern
|}]


let f (A x|B y) = 0
[%%expect {|
Line 1, characters 6-15:
1 | let f (A x|B y) = 0
          ^^^^^^^^^
Error: Variable x must occur on both sides of this | pattern
|}]

(** #t *)
type t = []
let f = function #t -> ()
[%%expect {|
type t = []
Line 2, characters 18-19:
2 | let f = function #t -> ()
                      ^
Error: The type t is not a variant type
|}]

let f {x;x=y;x=z} = x
[%%expect {|
Line 1, characters 6-17:
1 | let f {x;x=y;x=z} = x
          ^^^^^^^^^^^
Error: The record field label x is defined several times
|}]

(** Coercion failure *)

let x = ([`B]:>[`A])
[%%expect {|
Line 3, characters 9-13:
3 | let x = ([`B]:>[`A])
             ^^^^
Error: This expression cannot be coerced to type [ `A ]; it has type
         [> `B ] list
       but is here used with type [< `A ]
|}]

(** Unbound instance variable *)

let o = object method m = instance <- 0 end

[%%expect{|
Line 3, characters 26-39:
3 | let o = object method m = instance <- 0 end
                              ^^^^^^^^^^^^^
Error: Unbound instance variable instance
|}]


(** Hash collision *)
let x = function
  | `azdwbie -> ()
  | `c7diagq -> ()
[%%expect{|
Line 3, characters 4-12:
3 |   | `c7diagq -> ()
        ^^^^^^^^
Error: Variant tags `azdwbie and `c7diagq have the same hash value.
       Change one of them.
|}]


let x =  `azdwbie = `c7diagq
[%%expect{|
Line 1, characters 20-28:
1 | let x =  `azdwbie = `c7diagq
                        ^^^^^^^^
Error: Variant tags `azdwbie and `c7diagq have the same hash value.
       Change one of them.
|}]

type 'a x =
  | X: [>`azdwbie] x
  | Y: [>`c7diagq] x

let x  = function
  | X -> ()
  | Y  -> ()

[%%expect{|
type 'a x = X : [> `azdwbie ] x | Y : [> `c7diagq ] x
Line 7, characters 4-5:
7 |   | Y  -> ()
        ^
Error: Variant tags `azdwbie and `c7diagq have the same hash value.
       Change one of them.
|}]


type t = {x:unit}
type s = {y:unit}
let f = function {x; y} -> x
[%%expect {|
type t = { x : unit; }
type s = { y : unit; }
Line 3, characters 21-22:
3 | let f = function {x; y} -> x
                         ^
Error: The record field y belongs to the type s
       but is mixed here with fields of type t
|}]


(** Error extension node *)

let x = [%ocaml.error "Expression error"]
[%%expect {|
Line 3, characters 10-21:
3 | let x = [%ocaml.error "Expression error"]
              ^^^^^^^^^^^
Error: Expression error
|}]

let f [%ocaml.error "Pattern error"] = ()
[%%expect {|
Line 1, characters 8-19:
1 | let f [%ocaml.error "Pattern error"] = ()
            ^^^^^^^^^^^
Error: Pattern error
|}]
