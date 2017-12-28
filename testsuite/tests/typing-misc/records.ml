(* TEST
   * expect
*)

(* undefined labels *)
type t = {x:int;y:int};;
{x=3;z=2};;
[%%expect{|
type t = { x : int; y : int; }
Line _, characters 5-6:
  {x=3;z=2};;
       ^
Error: Unbound record field z
|}];;
fun {x=3;z=2} -> ();;
[%%expect{|
Line _, characters 9-10:
  fun {x=3;z=2} -> ();;
           ^
Error: Unbound record field z
|}];;

(* mixed labels *)
{x=3; contents=2};;
[%%expect{|
Line _, characters 6-14:
  {x=3; contents=2};;
        ^^^^^^^^
Error: The record field contents belongs to the type 'a ref
       but is mixed here with fields of type t
|}];;

(* private types *)
type u = private {mutable u:int};;
{u=3};;
[%%expect{|
type u = private { mutable u : int; }
Line _, characters 0-5:
  {u=3};;
  ^^^^^
Error: Cannot create values of the private type u
|}];;
fun x -> x.u <- 3;;
[%%expect{|
Line _, characters 11-12:
  fun x -> x.u <- 3;;
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
Line _, characters 17-18:
  let f (r: int) = r.y <- 3;;
                   ^
Error: This expression has type int but an expression was expected of type
         foo
|}];;

(* bugs *)
type foo = { y: int; z: int };;
type bar = { x: int };;
let f (r: bar) = ({ r with z = 3 } : foo)
[%%expect{|
type foo = { y : int; z : int; }
type bar = { x : int; }
Line _, characters 20-21:
  let f (r: bar) = ({ r with z = 3 } : foo)
                      ^
Error: This expression has type bar but an expression was expected of type
         foo
|}];;

type foo = { x: int };;
let r : foo = { ZZZ.x = 2 };;
[%%expect{|
type foo = { x : int; }
Line _, characters 16-21:
  let r : foo = { ZZZ.x = 2 };;
                  ^^^^^
Error: Unbound module ZZZ
|}];;

(ZZZ.X : int option);;
[%%expect{|
Line _, characters 1-6:
  (ZZZ.X : int option);;
   ^^^^^
Error: Unbound module ZZZ
|}];;

(* PR#5865 *)
let f (x : Complex.t) = x.Complex.z;;
[%%expect{|
Line _, characters 26-35:
  let f (x : Complex.t) = x.Complex.z;;
                            ^^^^^^^^^
Error: Unbound record field Complex.z
|}];;

(* PR#6608 *)
{ true with contents = 0 };;
[%%expect{|
Line _, characters 2-6:
  { true with contents = 0 };;
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

let no = { };;
[%%expect{|
Line _, characters 9-12:
  let no = { };;
           ^^^
Error: Unknown empty record type
|}];;

type m = { };;
let x = { }
type n = m
let y:n = x
module X = struct type g = { }  end
let g = { }
let n = let open X in { }
let h = n
type c = C of { } | D of m * e and e = {};;
let f = function C {} -> 1 | D ({}, {}) -> 2
let m, n = (f (C {})), (f (D ({}, {})));;
let g = function { } -> 2
[%%expect{|
type m = { }
val x : m = {}
type n = m
val y : n = {}
module X : sig type g = { } end
val g : m = {}
val n : X.g = {}
val h : X.g = {}
type c = C of { } | D of m * e
and e = { }
val f : c -> int = <fun>
val m : int = 1
val n : int = 2
val g : e -> int = <fun>
|}];;
