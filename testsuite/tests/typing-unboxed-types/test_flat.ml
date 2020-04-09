(* TEST
   * flat-float-array
   ** expect
*)

(* should fail *)
type 'a abs;;
type t16 = A : _ abs -> t16 [@@ocaml.unboxed];;
[%%expect{|
type 'a abs
Line 2, characters 0-45:
2 | type t16 = A : _ abs -> t16 [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* should fail (the existential _ still occurs in an abstract type) *)
type t18 = A : _ list abs -> t18 [@@ocaml.unboxed];;
[%%expect{|
Line 1, characters 0-50:
1 | type t18 = A : _ list abs -> t18 [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* regression test for PR#7511 (wrong determination of unboxability for GADTs)
*)
type 'a s = S : 'a -> 'a s [@@unboxed];;
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a s [@@unboxed]
Line 2, characters 0-33:
2 | type t = T : _ s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* regression test for GPR#1133 (follow-up to PR#7511) *)
type 'a s = S : 'a -> 'a option s [@@unboxed];;
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a option s [@@unboxed]
Line 2, characters 0-33:
2 | type t = T : _ s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values.
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
       it might contain both float and non-float values.
       You should annotate it with [@@ocaml.boxed].
|}];;

type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed];;
[%%expect{|
type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed]
|}];;

(* reject *)
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
Line 1, characters 0-33:
1 | type t = T : _ s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values.
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
Line 1, characters 0-39:
1 | type _ s = S : 'a t -> _ s  [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* regression test for PR#7511 (wrong determination of unboxability for GADTs)
*)
type 'a s = S : 'a -> 'a s [@@unboxed];;
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a s [@@unboxed]
Line 2, characters 0-33:
2 | type t = T : _ s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values.
       You should annotate it with [@@ocaml.boxed].
|}];;

(* regression test for GPR#1133 (follow-up to PR#7511) *)
type 'a s = S : 'a -> 'a option s [@@unboxed];;
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a option s [@@unboxed]
Line 2, characters 0-33:
2 | type t = T : _ s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values.
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
       it might contain both float and non-float values.
       You should annotate it with [@@ocaml.boxed].
|}];;

type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed];;
[%%expect{|
type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed]
|}];;

(* reject *)
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
Line 1, characters 0-33:
1 | type t = T : _ s -> t [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values.
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
Line 1, characters 0-39:
1 | type _ s = S : 'a t -> _ s  [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values.
       You should annotate it with [@@ocaml.boxed].
|}];;
