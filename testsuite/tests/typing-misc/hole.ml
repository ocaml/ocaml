(* TEST
 expect;
*)

(* "_" parses as an expression (a "hole", Pexp_hole) anywhere a simple
   expression is allowed, but is rejected by the type-checker: holes
   are intended to be eliminated by a ppx rewriter. *)

type r = { a : int }
let f (x : int) = x
let fx ~x = x
let g ~_:x = x
let o ?_:(x = 0) () = x
[%%expect{|
type r = { a : int; }
val f : int -> int = <fun>
val fx : x:'a -> 'a = <fun>
val g : _:'a -> 'a = <fun>
val o : ?_:int -> unit -> int = <fun>
|}]

(* "~_:" and "?_:" lex as labels named "_", so these do not involve
   holes at all. *)

let ok = g ~_:3
[%%expect{|
val ok : int = 3
|}]

let ok = o ?_:(Some 5) ()
[%%expect{|
val ok : int = 5
|}]

(* Holes where a general expression can start. *)

let x = _
[%%expect{|
Line 1, characters 8-9:
1 | let x = _
            ^
Error: Uninterpreted expression wildcard "_".
|}]

let p = (_, 0)
[%%expect{|
Line 1, characters 9-10:
1 | let p = (_, 0)
             ^
Error: Uninterpreted expression wildcard "_".
|}]

let r1 = { a = _ }
[%%expect{|
Line 1, characters 15-16:
1 | let r1 = { a = _ }
                   ^
Error: Uninterpreted expression wildcard "_".
|}]

let n = 1 + _
[%%expect{|
Line 1, characters 12-13:
1 | let n = 1 + _
                ^
Error: Uninterpreted expression wildcard "_".
|}]

let c = if _ then 0 else 1
[%%expect{|
Line 1, characters 11-12:
1 | let c = if _ then 0 else 1
               ^
Error: Uninterpreted expression wildcard "_".
|}]

let t = (_ : int)
[%%expect{|
Line 1, characters 9-10:
1 | let t = (_ : int)
             ^
Error: Uninterpreted expression wildcard "_".
|}]

let fn = fun () -> _
[%%expect{|
Line 1, characters 19-20:
1 | let fn = fun () -> _
                       ^
Error: Uninterpreted expression wildcard "_".
|}]

let ap = _ 0
[%%expect{|
Line 1, characters 9-10:
1 | let ap = _ 0
             ^
Error: Uninterpreted expression wildcard "_".
|}]

let fd = _.a
[%%expect{|
Line 1, characters 9-10:
1 | let fd = _.a
             ^
Error: Uninterpreted expression wildcard "_".
|}]

(* Holes in function-argument positions. *)

let a1 = f _
[%%expect{|
Line 1, characters 11-12:
1 | let a1 = f _
               ^
Error: Uninterpreted expression wildcard "_".
|}]

let a2 = Some _
[%%expect{|
Line 1, characters 14-15:
1 | let a2 = Some _
                  ^
Error: Uninterpreted expression wildcard "_".
|}]

let a3 = lazy _
[%%expect{|
Line 1, characters 14-15:
1 | let a3 = lazy _
                  ^
Error: Uninterpreted expression wildcard "_".
|}]

let a4 = fx ~x:_
[%%expect{|
Line 1, characters 15-16:
1 | let a4 = fx ~x:_
                   ^
Error: Uninterpreted expression wildcard "_".
|}]

let a5 = g ~_
[%%expect{|
Line 1, characters 12-13:
1 | let a5 = g ~_
                ^
Error: Uninterpreted expression wildcard "_".
|}]

let a6 = g ~_:_
[%%expect{|
Line 1, characters 14-15:
1 | let a6 = g ~_:_
                  ^
Error: Uninterpreted expression wildcard "_".
|}]

let a7 = o ?_
[%%expect{|
Line 1, characters 12-13:
1 | let a7 = o ?_
                ^
Error: Uninterpreted expression wildcard "_".
|}]

let a8 = o ?_:_
[%%expect{|
Line 1, characters 14-15:
1 | let a8 = o ?_:_
                  ^
Error: Uninterpreted expression wildcard "_".
|}]

(* "_" also parses as a module expression (a hole, Pmod_hole) and is
   likewise rejected by the type-checker. *)

module type S = sig end
module F (X : S) = struct end
[%%expect{|
module type S = sig end
module F : (X : S) -> sig end
|}]

module M = _
[%%expect{|
Line 1, characters 11-12:
1 | module M = _
               ^
Error: Uninterpreted module wildcard "_".
|}]

module N = F(_)
[%%expect{|
Line 1, characters 13-14:
1 | module N = F(_)
                 ^
Error: Uninterpreted module wildcard "_".
|}]

module O = (_ : S)
[%%expect{|
Line 1, characters 12-13:
1 | module O = (_ : S)
                ^
Error: Uninterpreted module wildcard "_".
|}]

include _
[%%expect{|
Line 1, characters 8-9:
1 | include _
            ^
Error: Uninterpreted module wildcard "_".
|}]

open _
[%%expect{|
Line 1, characters 5-6:
1 | open _
         ^
Error: Uninterpreted module wildcard "_".
|}]

module type P = module type of _
[%%expect{|
Line 1, characters 31-32:
1 | module type P = module type of _
                                   ^
Error: Uninterpreted module wildcard "_".
|}]

let x = (module _ : S)
[%%expect{|
Line 1, characters 16-17:
1 | let x = (module _ : S)
                    ^
Error: Uninterpreted module wildcard "_".
|}]

let y = let module L = _ in ()
[%%expect{|
Line 1, characters 23-24:
1 | let y = let module L = _ in ()
                           ^
Error: Uninterpreted module wildcard "_".
|}]
