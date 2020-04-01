(* TEST
   * flat-float-array
   ** expect
*)

(* should fail *)
type 'a abs;;
type t16 = A : 'a abs -> t16 [@@ocaml.unboxed];;
[%%expect{|
type 'a abs
Line 2, characters 0-46:
2 | type t16 = A : 'a abs -> t16 [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* should fail (the existential _ still occurs in an abstract type) *)
type t18 = A : _ list abs -> t18 [@@ocaml.unboxed];;
[%%expect{|
Line 1, characters 0-50:
1 | type t18 = A : _ list abs -> t18 [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of an unnamed existential variable.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* regression test for PR#7511 (wrong determination of unboxability for GADTs)
*)
type 'a s = S : 'a -> 'a s [@@unboxed];;
type t = T : 'a s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a s [@@unboxed]
Line 2, characters 0-34:
2 | type t = T : 'a s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* regression test for GPR#1133 (follow-up to PR#7511) *)
type 'a s = S : 'a -> 'a option s [@@unboxed];;
type t = T : 'a s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a option s [@@unboxed]
Line 2, characters 0-34:
2 | type t = T : 'a s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* Another test for GPR#1133: abstract types *)
module M : sig
  type 'a r constraint 'a = unit -> 'b
  val inj : 'b -> (unit -> 'b) r
end = struct
  type 'a r = 'b constraint 'a = unit -> 'b
  let inj x = x
end;;
[%%expect{|
module M :
  sig type 'a r constraint 'a = unit -> 'b val inj : 'b -> (unit -> 'b) r end
|}];;

(* reject *)
type t = T : (unit -> _) M.r -> t [@@unboxed];;
[%%expect{|
Line 1, characters 0-45:
1 | type t = T : (unit -> _) M.r -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of an unnamed existential variable.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* accept *)
type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed];;
[%%expect{|
type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed]
|}];;

(* reject *)
type t = T : 'a s -> t [@@unboxed];;
[%%expect{|
Line 1, characters 0-34:
1 | type t = T : 'a s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* accept *)
type 'a t = T : 'a s -> 'a t [@@unboxed];;
[%%expect{|
type 'a t = T : 'a s -> 'a t [@@unboxed]
|}];;

(* Even without constraints, we need to mark abstract types as Deepsep:
   unboxed GADTs can introduce equations that do not appear in the signature
   (see GPR#2188) *)
module N : sig
  type 'a r
  val inj : 'b -> (unit -> 'b) r
end = struct
  type _ r = K : 'b -> (unit -> 'b) r [@@unboxed]
  let inj x = K x
end;;
[%%expect{|
module N : sig type 'a r val inj : 'b -> (unit -> 'b) r end
|}];;

(* reject *)
type t = T : (unit -> _) N.r -> t [@@unboxed];;
[%%expect{|
Line 1, characters 0-45:
1 | type t = T : (unit -> _) N.r -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of an unnamed existential variable.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* accept *)
type 'a s = S : (unit -> 'a) N.r -> 'a option s [@@unboxed];;
[%%expect{|
type 'a s = S : (unit -> 'a) N.r -> 'a option s [@@unboxed]
|}];;

(* Another corner case from GPR#1133 *)
type _ s = S : 'a t -> _ s  [@@unboxed]
 and _ t = T : 'a -> 'a s t
;;
[%%expect{|
type _ s = S : 'a t -> 'b s [@@unboxed]
and _ t = T : 'a -> 'a s t
|}];;

(* regression test for PR#7511 (wrong determination of unboxability for GADTs)
*)
type 'a s = S : 'a -> 'a s [@@unboxed];;
type t = T : 'a s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a s [@@unboxed]
Line 2, characters 0-34:
2 | type t = T : 'a s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* regression test for GPR#1133 (follow-up to PR#7511) *)
type 'a s = S : 'a -> 'a option s [@@unboxed];;
type t = T : 'a s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a option s [@@unboxed]
Line 2, characters 0-34:
2 | type t = T : 'a s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* Another test for GPR#1133: abstract types *)
module M : sig
  type 'a r constraint 'a = unit -> 'b
  val inj : 'b -> (unit -> 'b) r
end = struct
  type 'a r = 'b constraint 'a = unit -> 'b
  let inj x = x
end;;
[%%expect{|
module M :
  sig type 'a r constraint 'a = unit -> 'b val inj : 'b -> (unit -> 'b) r end
|}];;

(* reject *)
type t = T : (unit -> _) M.r -> t [@@unboxed];;
[%%expect{|
Line 1, characters 0-45:
1 | type t = T : (unit -> _) M.r -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of an unnamed existential variable.
       You should annotate it with [@@ocaml.boxed].
|}];;

type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed];;
[%%expect{|
type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed]
|}];;

(* reject *)
type t = T : 'a s -> t [@@unboxed];;
[%%expect{|
Line 1, characters 0-34:
1 | type t = T : 'a s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* accept *)
type 'a t = T : 'a s -> 'a t [@@unboxed];;
[%%expect{|
type 'a t = T : 'a s -> 'a t [@@unboxed]
|}];;


(* Another corner case from GPR#1133 *)
type _ s = S : 'a t -> _ s  [@@unboxed]
 and _ t = T : 'a -> 'a s t
;;
[%%expect{|
type _ s = S : 'a t -> 'b s [@@unboxed]
and _ t = T : 'a -> 'a s t
|}];;

(* GPR#2188: non-principality examples.
   One of the two declarations [valid1] and [valid2] below will fail,
   depending on the order in which GADT equality constraints
   are processed by our implementation.
   The previous unfolding implementation would accept both.

   We decided to specify that, if two parameters are equal to each other,
   then we would use the more constrained mode (Sep rather than Ind) for the
   first/leftmost parameter, and Ind for the second one. With a left-to-right
   reading of parameters, this corresponds to considering that the equality
   is on the second parameter, equal to a parameter already seen, rather than
   an equality on a not-yet-seen parameter.

   In the example below, almost_eq will thus get the mode signature
   (Sep, Ind) rather than (Ind, Sep).
*)
type (_, _) almost_eq = Almost_refl : 'a -> ('a, 'a) almost_eq [@@unboxed]
[%%expect{|
type (_, _) almost_eq = Almost_refl : 'a -> ('a, 'a) almost_eq [@@unboxed]
|}];;


type valid1 = Any : ('a, int) almost_eq -> valid1 [@@unboxed];;
[%%expect{|
Line 1, characters 0-61:
1 | type valid1 = Any : ('a, int) almost_eq -> valid1 [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}];;
type valid2 = Any : (int, 'a) almost_eq -> valid2 [@@unboxed];;
[%%expect{|
type valid2 = Any : (int, 'a) almost_eq -> valid2 [@@unboxed]
|}];;

(* rejected: equivalent to (exits 'a. 'a) *)
type danger = Any : ('a, 'a) almost_eq -> danger [@@unboxed];;
[%%expect{|
Line 1, characters 0-60:
1 | type danger = Any : ('a, 'a) almost_eq -> danger [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable 'a.
       You should annotate it with [@@ocaml.boxed].
|}];;


(* GPR#2188: handling of cyclic types *)
type 'a stream = unit -> [ `Cons of 'a * 'a stream ];;
type safe = Any : 'a stream -> safe;;
[%%expect{|
type 'a stream = unit -> [ `Cons of 'a * 'a stream ]
type safe = Any : 'a stream -> safe
|}];;

type 'a infinite_full_tree = unit -> [ `Node of 'a * ('a * 'a) stream ];;
type safe_again = Any : 'a stream -> safe_again;;
[%%expect{|
type 'a infinite_full_tree = unit -> [ `Node of 'a * ('a * 'a) stream ]
type safe_again = Any : 'a stream -> safe_again
|}];;

(** Note: there are no tests of rejected cyclic types, because
    the type declarations that would be required to check these cases
    (unproductive cycles in the type declaration) are already rejected by the
    type-checker, before separability checking. See below *)
type 'a id = Id of 'a [@@unboxed]
type cycle = cycle id
[%%expect{|
type 'a id = Id of 'a [@@unboxed]
Line 2, characters 0-21:
2 | type cycle = cycle id
    ^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation cycle is cyclic
|}];;
