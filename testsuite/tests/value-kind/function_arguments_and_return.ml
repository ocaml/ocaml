(* TEST
   flags = "-dlambda -dno-unique-ids"
   * expect *)

type t1 = A of int * float
let f1 (x:t1) = x
[%%expect{|
0
type t1 = A of int * float
(let (f1 = (function x[0: int, float] : [0: int, float] x))
  (apply (field 1 (global Toploop!)) "f1" f1))
val f1 : t1 -> t1 = <fun>
|}]

type t2 = { a : int; b : float }
let f2 (x:t2) = x
[%%expect{|
0
type t2 = { a : int; b : float; }
(let (f2 = (function x[0: int, float] : [0: int, float] x))
  (apply (field 1 (global Toploop!)) "f2" f2))
val f2 : t2 -> t2 = <fun>
|}]

type t3 = { a : float; b : float }
let f3 (x:t3) = x
[%%expect{|
0
type t3 = { a : float; b : float; }
(let (f3 = (function x[254: float, float] : [254: float, float] x))
  (apply (field 1 (global Toploop!)) "f3" f3))
val f3 : t3 -> t3 = <fun>
|}]

type t4 = A of { a : int; b : float }
let f4 (x:t4) = x
let g4 a b = A { a; b }
[%%expect{|
0
type t4 = A of { a : int; b : float; }
(let (f4 = (function x[0: int, float] : [0: int, float] x))
  (apply (field 1 (global Toploop!)) "f4" f4))
val f4 : t4 -> t4 = <fun>
(let
  (g4 =
     (function a[int] b[float] : [0: int, float]
       (makeblock 0 (int,float) a b)))
  (apply (field 1 (global Toploop!)) "g4" g4))
val g4 : int -> float -> t4 = <fun>
|}]

type t5 = A of float [@@unboxed]
let f5 (x:t5) = x
[%%expect{|
0
type t5 = A of float [@@unboxed]
(let (f5 = (function x[float] : float x))
  (apply (field 1 (global Toploop!)) "f5" f5))
val f5 : t5 -> t5 = <fun>
|}]

type t6 = A | B | C
let f6 (x:t6) = x
[%%expect{|
0
type t6 = A | B | C
(let (f6 = (function x[int] : int x))
  (apply (field 1 (global Toploop!)) "f6" f6))
val f6 : t6 -> t6 = <fun>
|}]

type t7 =
  | A of float
  | B of int
let f7 (x:t7) = x
[%%expect{|
0
type t7 = A of float | B of int
(let (f7 = (function x x)) (apply (field 1 (global Toploop!)) "f7" f7))
val f7 : t7 -> t7 = <fun>
|}]

type t8 =
  | A of float
  | B
let f8 (x:t8) = x
[%%expect{|
0
type t8 = A of float | B
(let (f8 = (function x x)) (apply (field 1 (global Toploop!)) "f8" f8))
val f8 : t8 -> t8 = <fun>
|}]

type t9 = { mutable a : int; b : float }
let f9 (x:t9) = x
[%%expect{|
0
type t9 = { mutable a : int; b : float; }
(let (f9 = (function x x)) (apply (field 1 (global Toploop!)) "f9" f9))
val f9 : t9 -> t9 = <fun>
|}]

type t10 = A of { mutable a : int; b : float }
let f10 (x:t10) = x
[%%expect{|
0
type t10 = A of { mutable a : int; b : float; }
(let (f10 = (function x x)) (apply (field 1 (global Toploop!)) "f10" f10))
val f10 : t10 -> t10 = <fun>
|}]

type t11 = { a : t1; b : t6 }
let f11 (x:t11) = x
[%%expect{|
0
type t11 = { a : t1; b : t6; }
(let
  (f11 = (function x[0: [0: int, float], int] : [0: [0: int, float], int] x))
  (apply (field 1 (global Toploop!)) "f11" f11))
val f11 : t11 -> t11 = <fun>
|}]

type t12 = (int * t3)
let f12 (x:t12) = x
[%%expect{|
0
type t12 = int * t3
(let
  (f12 =
     (function x[0: int, [254: float, float]] : [0: int, [254: float, float]]
       x))
  (apply (field 1 (global Toploop!)) "f12" f12))
val f12 : t12 -> t12 = <fun>
|}]

type t13 = ..
let f13 (x:t13) = x
[%%expect{|
0
type t13 = ..
(let (f13 = (function x x)) (apply (field 1 (global Toploop!)) "f13" f13))
val f13 : t13 -> t13 = <fun>
|}]

type t14 = [`A of int]
let f14 (x:t14) = x
[%%expect{|
0
type t14 = [ `A of int ]
(let (f14 = (function x x)) (apply (field 1 (global Toploop!)) "f14" f14))
val f14 : t14 -> t14 = <fun>
|}]

type t15 = { a : int; b : t15 }
let f15 (x:t15) = x
[%%expect{|
0
type t15 = { a : int; b : t15; }
(let (f15 = (function x[0: int, [0: int, *]] : [0: int, [0: int, *]] x))
  (apply (field 1 (global Toploop!)) "f15" f15))
val f15 : t15 -> t15 = <fun>
|}]

type t16 = (int * t16')
and t16' = A of t16
let f16 (x:t16) = x
[%%expect{|
0
type t16 = int * t16'
and t16' = A of t16
(let (f16 = (function x[0: int, [0: *]] : [0: int, [0: *]] x))
  (apply (field 1 (global Toploop!)) "f16" f16))
val f16 : t16 -> t16 = <fun>
|}]

let f17 (x:unit) = x
[%%expect{|
(let (f17 = (function x[int] : int x))
  (apply (field 1 (global Toploop!)) "f17" f17))
val f17 : unit -> unit = <fun>
|}]

(* Recursive so that 'scrape' returns a Record_unboxed representation *)
type trec = { t : trec } [@@unboxed]
let f18 { t } = t
[%%expect{|
0
type trec = { t : trec; } [@@unboxed]
(let (f18 = (function param param))
  (apply (field 1 (global Toploop!)) "f18" f18))
val f18 : trec -> trec = <fun>
|}]

type trec2 = A of trec2 [@@unboxed]
let f19 (A t) = t
[%%expect{|
0
type trec2 = A of trec2 [@@unboxed]
(let (f19 = (function param param))
  (apply (field 1 (global Toploop!)) "f19" f19))
val f19 : trec2 -> trec2 = <fun>
|}]
