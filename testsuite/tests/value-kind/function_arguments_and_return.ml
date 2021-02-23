(* TEST
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
      flags = "-dlambda -dno-unique-ids"
   *** check-ocamlc.byte-output
*)

type t1 = A of int * float
let f1 (x:t1) = x

type t2 = { a : int; b : float }
let f2 (x:t2) = x

type t3 = { a : float; b : float }
let f3 (x:t3) = x

type t4 = A of { a : int; b : float }
let f4 (x:t4) = x
let g4 a b = A { a; b }

type t5 = A of float [@@unboxed]
let f5 (x:t5) = x

type t6 = A | B | C
let f6 (x:t6) = x

type t7 =
  | A of float
  | B of int
let f7 (x:t7) = x

type t8 =
  | A of float
  | B
let f8 (x:t8) = x

type t9 = { mutable a : int; b : float }
let f9 (x:t9) = x

type t10 = A of { mutable a : int; b : float }
let f10 (x:t10) = x

type t11 = { a : t1; b : t6 }
let f11 (x:t11) = x

type t12 = (int * t3)
let f12 (x:t12) = x

type t13 = ..
let f13 (x:t13) = x

type t14 = [`A of int]
let f14 (x:t14) = x

type t15 = { a : int; b : t15 }
let f15 (x:t15) = x

type t16 = (int * t16')
and t16' = A of t16
let f16 (x:t16) = x

let f17 (x:unit) = x
