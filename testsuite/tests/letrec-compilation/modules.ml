(* TEST *)

module type A = sig
  val a : int
  val b : int
end

module type B = sig
  val b : int
end

module F(A:A) = struct
  let a = 1
  let b = A.a + A.b
end

module Ax = struct let a = 33 let b = 44 end
let rec a =
  let _ = (a,a) in
  (module F (Ax) : A)

let rec a =
  let _ = (a,a) in
  (module (F (struct let a = 33 let b = 44 end)) : A)
and b =
  let exception E : 'a -> exn in
  E b
