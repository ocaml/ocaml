(* TEST
 expect;
*)


(* a regression from Brandon Stride *)

type[@unboxed] 'a foo = { foo : 'b } constraint 'a = 'b * 'c
[%%expect{|
type 'a foo = { foo : 'b; } constraint 'a = 'b * 'c [@@unboxed]
|}];;

let bar : 'c. (unit * 'c) foo = { foo = () }
[%%expect{|
val bar : (unit * 'c) foo = {foo = ()}
|}];;


(* an extension of the regression by Stefan Muenzel *)

type [@unboxed] 'a t1 = { foo' : 'c . ('a * 'c) foo }
[%%expect{|
type 'a t1 = { foo' : 'c. ('a * 'c) foo; } [@@unboxed]
|}]

type 'a t0 = 'a t1
[%%expect{|
type 'a t0 = 'a t1
|}]

type 'a t2 = { t0 : 'a t0 }
[%%expect{|
type 'a t2 = { t0 : 'a t0; }
|}]
