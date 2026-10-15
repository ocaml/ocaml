(* TEST
 flags = "-i-variance";
 expect;
*)

type 'a q = Q
type (_, _) eq = Refl : ('a, 'a) eq

module F (X : sig type 'a t end) : sig
  type +-'a s
  val inj : 'a q X.t -> 'a s
  val prj : 'a s -> 'a q X.t
end = struct
  type 'a s = 'a q X.t
  let inj x = x
  let prj x = x
end

let magic (type a b) (x : a) : b =
  let module M = F (struct type 'a t = ('a, a q) eq end) in
  let Refl = M.prj (M.inj Refl : a M.s :> b M.s) in
  x

[%%expect{|
type +-!'a q = Q
type (!_, !_) eq = Refl : ('a, 'a) eq
Lines 8-12, characters 6-3:
 8 | ......struct
 9 |   type 'a s = 'a q X.t
10 |   let inj x = x
11 |   let prj x = x
12 | end
Error: Signature mismatch:
       Modules do not match:
         sig type !'a s = 'a q X.t val inj : 'a -> 'a val prj : 'a -> 'a end
       is not included in
         sig
           type +-'a s
           val inj : 'a q X.t -> 'a s
           val prj : 'a s -> 'a q X.t
         end
       Type declarations do not match:
         type !'a s = 'a q X.t
       is not included in
         type +-'a s
       Their variances do not agree.
Unexecuted phrases: 1 phrases did not execute due to an error
|}]
