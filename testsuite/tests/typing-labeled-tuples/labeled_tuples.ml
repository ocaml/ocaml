(* TEST
   expect;
*)

(* Basic expressions *)
let x = ~x:1, ~y:2

[%%expect{|
val x : x:int * y:int = (~x:1, ~y:2)
|}];;

let z = 5
let punned = 2
let _ = ~x: 5, 2, ~z, ~(punned:int)
[%%expect{|
val z : int = 5
val punned : int = 2
- : x:int * int * z:int * punned:int = (~x:5, 2, ~z:5, ~punned:2)
|}]

(* Basic annotations *)
let (x : x:int * y:int) = ~x:1, ~y:2
[%%expect{|
val x : x:int * y:int = (~x:1, ~y:2)
|}]

let (x : x:int * int) = ~x:1, 2
[%%expect{|
val x : x:int * int = (~x:1, 2)
|}]

(* Incorrect annotations *)
let (x : int * int) = ~x:1, 2
[%%expect{|
Line 1, characters 22-29:
1 | let (x : int * int) = ~x:1, 2
                          ^^^^^^^
Error: This expression has type "x:'a * 'b"
       but an expression was expected of type "int * int"
|}]

let (x : x:string * int) = ~x:1, 2
[%%expect{|
Line 1, characters 30-31:
1 | let (x : x:string * int) = ~x:1, 2
                                  ^
Error: The constant "1" has type "int" but an expression was expected of type
         "string"
|}]

let (x : int * y:int) = ~x:1, 2
[%%expect{|
Line 1, characters 24-31:
1 | let (x : int * y:int) = ~x:1, 2
                            ^^^^^^^
Error: This expression has type "x:'a * 'b"
       but an expression was expected of type "int * y:int"
|}]

(* Happy case *)
let foo b = if b then
   ~a: "s", 10, ~c: "hi"
else
   ~a: "5", 10, ~c: "hi"
[%%expect{|
val foo : bool -> a:string * int * c:string = <fun>
|}]

(* Missing label (the type vars in the error aren't ideal, but the same thing
   happens when unifying normal tuples of different lengths) *)
let foo b = if b then
   ~a: "s", 10, "hi"
else
   ~a: "5", 10, ~c: "hi"
[%%expect{|
Line 4, characters 3-24:
4 |    ~a: "5", 10, ~c: "hi"
       ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a:string * int * c:'a"
       but an expression was expected of type "a:string * int * string"
|}]

(* Missing labeled component *)
let foo b = if b then
   ~a: "s", 10
else
   ~a: "5", 10, ~c: "hi"
[%%expect{|
Line 4, characters 3-24:
4 |    ~a: "5", 10, ~c: "hi"
       ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a:'a * 'b * c:'c"
       but an expression was expected of type "a:string * int"
|}]

(* Wrong label *)
let foo b = if b then
   ~a: "s", 10, ~b: "hi"
else
   ~a: "5", 10, ~c: "hi"
[%%expect{|
Line 4, characters 3-24:
4 |    ~a: "5", 10, ~c: "hi"
       ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a:string * int * c:'a"
       but an expression was expected of type "a:string * int * b:string"
|}]

(* Repeated labels *)
type t = x:int * bool * x:int
[%%expect{|
Line 1, characters 9-29:
1 | type t = x:int * bool * x:int
             ^^^^^^^^^^^^^^^^^^^^
Error: This tuple type has two labels named "x"
|}]

let _ = 1, ~x:2, 3, ~x:4
[%%expect{|
Line 1, characters 8-24:
1 | let _ = 1, ~x:2, 3, ~x:4
            ^^^^^^^^^^^^^^^^
Error: This tuple expression has two labels named "x"
|}]

let f (a, ~x, b, ~x:c) = ()
[%%expect{|
Line 1, characters 6-22:
1 | let f (a, ~x, b, ~x:c) = ()
          ^^^^^^^^^^^^^^^^
Error: This tuple pattern has two labels named "x"
|}]

(* Types in function argument/return *)
let default = ~x: 1, ~y: 2
let choose_pt replace_with_default pt =
   if replace_with_default then
      default
   else
      pt
[%%expect{|
val default : x:int * y:int = (~x:1, ~y:2)
val choose_pt : bool -> (x:int * y:int) -> x:int * y:int = <fun>
|}]

(* Application happy case *)
let a = choose_pt true (~x: 5, ~y: 6)
[%%expect{|
val a : x:int * y:int = (~x:1, ~y:2)
|}]

(* Wrong order *)
let a = choose_pt true (~y: 6, ~x: 5)
[%%expect{|
Line 1, characters 23-37:
1 | let a = choose_pt true (~y: 6, ~x: 5)
                           ^^^^^^^^^^^^^^
Error: This expression has type "y:'a * x:'b"
       but an expression was expected of type "x:int * y:int"
|}]

(* Mutually-recursive definitions *)
let rec a = 1, ~lbl:b
and b = 2, ~lbl:a
[%%expect{|
Line 2, characters 16-17:
2 | and b = 2, ~lbl:a
                    ^
Error: The value "a" has type "int * lbl:(int * lbl:'a)"
       but an expression was expected of type "'a"
       The type variable "'a" occurs inside "int * lbl:(int * lbl:'a)"
|}]

let rec l = ~lbl: 5, ~lbl2: 10 :: l
[%%expect{|
val l : (lbl:int * lbl2:int) list = [(~lbl:5, ~lbl2:10); <cycle>]
|}]

(* Tuple containing labeled tuples *)
let tup = (~a:1, ~b:2), (~b:3, ~a:4), 5
[%%expect{|
val tup : (a:int * b:int) * (b:int * a:int) * int =
  ((~a:1, ~b:2), (~b:3, ~a:4), 5)
|}]

(* Polymorphic variant containing labeled tuple *)
let a = `Some (~a: 1, ~b:2, 3)
[%%expect{|
val a : [> `Some of a:int * b:int * int ] = `Some (~a:1, ~b:2, 3)
|}]

(* List of labeled tuples *)
let lst = ~a: 1, ~b: 2 :: []
[%%expect{|
val lst : (a:int * b:int) list = [(~a:1, ~b:2)]
|}]

(* Ref of labeled tuple *)
let x = ref (~x:"hello", 5)
[%%expect{|
val x : (x:string * int) ref = {contents = (~x:"hello", 5)}
|}]

(* Polymorphic record containing a labeled tuple *)
type 'a box = {thing: 'a}
let boxed = {thing = "hello", ~x:5}
[%%expect{|
type 'a box = { thing : 'a; }
val boxed : (string * x:int) box = {thing = ("hello", ~x:5)}
|}]

(* Punned tuple components with type annotations. *)
let x = 42
let y = "hi"

let z = ~x, ~(y:string);;
[%%expect{|
val x : int = 42
val y : string = "hi"
val z : x:int * y:string = (~x:42, ~y:"hi")
|}];;

let z = ~(x:int), ~y:"baz";;
[%%expect{|
val z : x:int * y:string = (~x:42, ~y:"baz")
|}];;

let z = ~(x:string), ~y:"baz";;
[%%expect{|
Line 1, characters 10-11:
1 | let z = ~(x:string), ~y:"baz";;
              ^
Error: The value "x" has type "int" but an expression was expected of type "string"
|}];;

(* Take a [a:'a * b:'a] and an int, and returns a
   [swapped:[a:'a * b:'a] * same:bool].
   The swapped component is the input with the [a] and [b] components swapped
   as many times as the input int. The second component is whether the first
   equals the input. *)
let rec swap (~a, ~b) =
   function
   | 0 -> ~swapped:(~a, ~b), ~same:true
   | n -> swap' (~a:b, ~b:a) (n-1)
and swap' (~a, ~b) =
   function
   | 0 -> ~swapped:(~a, ~b), ~same:false
   | n -> swap (~a:b, ~b:a) (n-1)
[%%expect{|
val swap : (a:'a * b:'a) -> int -> swapped:(a:'a * b:'a) * same:bool = <fun>
val swap' : (a:'a * b:'a) -> int -> swapped:(a:'a * b:'a) * same:bool = <fun>
|}]

let foobar = swap (~a:"foo", ~b:"bar") 86
let barfoo = swap (~a:"foo", ~b:"bar") 87
[%%expect{|
val foobar : swapped:(a:string * b:string) * same:bool =
  (~swapped:(~a:"foo", ~b:"bar"), ~same:true)
val barfoo : swapped:(a:string * b:string) * same:bool =
  (~swapped:(~a:"bar", ~b:"foo"), ~same:false)
|}]

(* Labeled tuple type annotations *)
(* Bad type *)
let x: string * a:int * int = ~lbl:5, "hi"
[%%expect{|
Line 1, characters 30-42:
1 | let x: string * a:int * int = ~lbl:5, "hi"
                                  ^^^^^^^^^^^^
Error: This expression has type "lbl:'a * 'b"
       but an expression was expected of type "string * a:int * int"
|}]

(* Well-typed *)
let x: string * a:int * int = "hi", ~a:1, 2
[%%expect{|
val x : string * a:int * int = ("hi", ~a:1, 2)
|}]

(* Function type *)
let mk_x : (foo:unit * bar:unit) -> string * a:int * int = fun _ -> x
[%%expect{|
val mk_x : (foo:unit * bar:unit) -> string * a:int * int = <fun>
|}]

let x = mk_x (~foo:(), ~bar:())
[%%expect{|
val x : string * a:int * int = ("hi", ~a:1, 2)
|}]

(* Labeled tuples in records *)

type bad_t = {x : lbl:bad_type * int}
[%%expect{|
Line 1, characters 22-30:
1 | type bad_t = {x : lbl:bad_type * int}
                          ^^^^^^^^
Error: Unbound type constructor "bad_type"
Hint: Did you mean "bad_t"?
|}]

type tx = { x : foo:int * bar:int }
type tx_unlabeled = { x : int * int }
[%%expect{|
type tx = { x : foo:int * bar:int; }
type tx_unlabeled = { x : int * int; }
|}]


let _ = { x = ~foo:1, ~bar:2}
[%%expect{|
Line 1, characters 14-28:
1 | let _ = { x = ~foo:1, ~bar:2}
                  ^^^^^^^^^^^^^^
Error: This expression has type "foo:'a * bar:'b"
       but an expression was expected of type "int * int"
|}]

let _ : tx = { x = ~foo:1, ~bar:2 }
[%%expect{|
- : tx = {x = (~foo:1, ~bar:2)}
|}]

let _ : tx = {x = 1, ~bar:2}
[%%expect{|
Line 1, characters 18-27:
1 | let _ : tx = {x = 1, ~bar:2}
                      ^^^^^^^^^
Error: This expression has type "'a * bar:'b"
       but an expression was expected of type "foo:int * bar:int"
|}]

let _ : tx = { x = ~foo:1, 2}
[%%expect{|
Line 1, characters 19-28:
1 | let _ : tx = { x = ~foo:1, 2}
                       ^^^^^^^^^
Error: This expression has type "foo:int * 'a"
       but an expression was expected of type "foo:int * bar:int"
|}]

let _ : tx = { x = 1, 2}
[%%expect{|
Line 1, characters 19-23:
1 | let _ : tx = { x = 1, 2}
                       ^^^^
Error: This expression has type "'a * 'b"
       but an expression was expected of type "foo:int * bar:int"
|}]

let _ = { x = 1, 2 }
[%%expect{|
- : tx_unlabeled = {x = (1, 2)}
|}]

(* Module inclusion *)

module IntString : sig
   type t
   val mk : (x: int * string) -> t
   val unwrap : t -> x:int * string
end = struct
  type t = string * x:int
  let mk (~x, s) = s, ~x
  let unwrap (s, ~x) = ~x, s
end
[%%expect{|
module IntString :
  sig
    type t
    val mk : (x:int * string) -> t
    val unwrap : t -> x:int * string
  end
|}]

module Stringable = struct
   module type Has_unwrap = sig
      type t
      val unwrap : t -> x: int * string
   end

   module type Has_to_string = sig
      include Has_unwrap
      val to_string : t -> string
   end

   module Make (M : Has_unwrap) : Has_to_string with type t := M.t = struct
      include M
      let to_string int_string =
         let (~x, s) = unwrap int_string in
         (Int.to_string x) ^ " " ^ s
   end
end
[%%expect{|
module Stringable :
  sig
    module type Has_unwrap = sig type t val unwrap : t -> x:int * string end
    module type Has_to_string =
      sig
        type t
        val unwrap : t -> x:int * string
        val to_string : t -> string
      end
    module Make :
      (M : Has_unwrap) ->
        sig
          val unwrap : M.t -> x:int * string
          val to_string : M.t -> string
        end
  end
|}]

module StringableIntString = struct
  module T = struct
    include IntString
  end
  include T
  include Stringable.Make(T)
end
[%%expect{|
module StringableIntString :
  sig
    module T :
      sig
        type t = IntString.t
        val mk : (x:int * string) -> t
        val unwrap : t -> x:int * string
      end
    type t = IntString.t
    val mk : (x:int * string) -> t
    val unwrap : T.t -> x:int * string
    val to_string : T.t -> string
  end
|}]

let _ = StringableIntString.to_string (StringableIntString.mk (~x:1, "hi"))
[%%expect{|
- : string = "1 hi"
|}]

module M : sig
  val f : (x:int * string) -> x:int * string
  val mk : unit -> x:bool * y:string
end = struct
  let f x = x
  let mk () = ~x:false, ~y:"hi"
end

(* Module inclusion failure *)
module X_int_int = struct
   type t = x:int * int
end
[%%expect{|
module M :
  sig
    val f : (x:int * string) -> x:int * string
    val mk : unit -> x:bool * y:string
  end
module X_int_int : sig type t = x:int * int end
|}]

module Y_int_int : sig
   type t = y:int * int
end = struct
   include X_int_int
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |    include X_int_int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = x:int * int end
       is not included in
         sig type t = y:int * int end
       Type declarations do not match:
         type t = x:int * int
       is not included in
         type t = y:int * int
       The type "x:int * int" is not equal to the type "y:int * int"
|}]

module Int_int : sig
   type t = int * int
end = X_int_int
[%%expect{|
Line 3, characters 6-15:
3 | end = X_int_int
          ^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = x:int * int end
       is not included in
         sig type t = int * int end
       Type declarations do not match:
         type t = x:int * int
       is not included in
         type t = int * int
       The type "x:int * int" is not equal to the type "int * int"
|}]

(* Recursive modules *)
module rec Tree : sig
   type t = Leaf of string | Branch of string * TwoTrees.t
   val in_order : t -> string list
end = struct
   type t = Leaf of string | Branch of string * TwoTrees.t
   let rec in_order = function
   | Leaf s -> [s]
   | Branch (s, (~left, ~right)) -> (in_order left) @ [s] @ (in_order right)
end
and TwoTrees : sig
   type t = left:Tree.t * right:Tree.t
end = struct
   type t = left:Tree.t * right:Tree.t
end
[%%expect{|
module rec Tree :
  sig
    type t = Leaf of string | Branch of string * TwoTrees.t
    val in_order : t -> string list
  end
and TwoTrees : sig type t = left:Tree.t * right:Tree.t end
|}]

let leaf s = Tree.Leaf s
let tree_abc = Tree.Branch ("b", (~left:(leaf "a"), ~right:(leaf "c")))
let tree_abcde = Tree.Branch ("d", (~left:tree_abc, ~right:(leaf "e")))
let _ = Tree.in_order tree_abcde
[%%expect{|
val leaf : string -> Tree.t = <fun>
val tree_abc : Tree.t =
  Tree.Branch ("b", (~left:Tree.Leaf "a", ~right:Tree.Leaf "c"))
val tree_abcde : Tree.t =
  Tree.Branch ("d",
   (~left:Tree.Branch ("b", (~left:Tree.Leaf "a", ~right:Tree.Leaf "c")),
    ~right:Tree.Leaf "e"))
- : string list = ["a"; "b"; "c"; "d"; "e"]
|}]

(* Motivating example *)
let sum_and_product ints =
  let init = ~sum:0, ~product:1 in
  List.fold_left (fun (~sum, ~product) elem ->
    let sum = elem + sum in
    let product = elem * product in
    ~sum, ~product
  ) init ints
let _ = sum_and_product [1;2;3;4]
let _ = sum_and_product [1;-10;2;]
[%%expect{|
val sum_and_product : int list -> sum:int * product:int = <fun>
- : sum:int * product:int = (~sum:10, ~product:24)
- : sum:int * product:int = (~sum:-7, ~product:-20)
|}]
