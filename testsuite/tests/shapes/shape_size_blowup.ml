(* TEST *)

(* Note: we do *not* enable -dshapes, as in this example
   shape sizs grow exponentially in the size of the M1..M7 family below. *)

module type S0 = sig
  type key
  type value

  type z1
  type z2
  type z3
  type z4
  type z5

  type z6
  type z7
  type z8
  type z9
  type z10
end

module M0 = struct
  type key
  type value

  type z1
  type z2
  type z3
  type z4
  type z5

  type z6
  type z7
  type z8
  type z9
  type z10
end

module type S0' = sig
  include S0
  type additional
end

(* note: our terms M{n} use a coercion from S0' to S0,
   which avoids the 'identity coercion' fast path in includemod;
   removing the 'additional' field from S0' above makes the
   -dshape output smaller (from exponential to constant). *)
module type S1 = (S0 -> S0') -> S0
module M1 : S1 = functor (P1 : S0 -> S0') -> P1(M0)

module type S2 = (S1 -> S0') -> S0
module M2 : S2 = functor (P1 : S1 -> S0') -> P1(M1)

module type S3 = (S2 -> S0') -> S0
module M3 : S3 = functor (P2 : S2 -> S0') -> P2(M2)

module type S4 = (S3 -> S0') -> S0
module M4 : S4 = functor (P3 : S3 -> S0') -> P3(M3)

module type S5 = (S4 -> S0') -> S0
module M5 : S5 = functor (P4 : S4 -> S0') -> P4(M4)

module type S6 = (S5 -> S0') -> S0
module M6 : S6 = functor (P5 : S5 -> S0') -> P5(M5)

module type S7 = (S6 -> S0') -> S0
module M7 : S7 = functor (P6 : S6 -> S0') -> P6(M6)
