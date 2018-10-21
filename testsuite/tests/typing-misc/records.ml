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
Error: This expression has type bool but an expression was expected of type
         'a ref
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
