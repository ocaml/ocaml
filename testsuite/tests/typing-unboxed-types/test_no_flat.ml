(* TEST
   * no-flat-float-array
   ** expect
*)

(* This file copies the tests from test_flat.ml,
   but is only tested when -no-flat-float-array
   is set, and thus all types are unboxable.

   We kept the comments on why each test should fail,
   to make it easier to compare the two files,
   but the test in this file should all pass,
   as shown in the expected outputs.
*)

(* should fail *)
type 'a abs;;
type t16 = A : _ abs -> t16 [@@ocaml.unboxed];;
[%%expect{|
type 'a abs
type t16 = A : 'a abs -> t16 [@@unboxed]
|}];;

(* should fail (the existential _ still occurs in an abstract type) *)
type t18 = A : _ list abs -> t18 [@@ocaml.unboxed];;
[%%expect{|
type t18 = A : 'a list abs -> t18 [@@unboxed]
|}];;

(* regression test for PR#7511 (wrong determination of unboxability for GADTs)
*)
type 'a s = S : 'a -> 'a s [@@unboxed];;
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a s [@@unboxed]
type t = T : 'a s -> t [@@unboxed]
|}];;

(* regression test for GPR#1133 (follow-up to PR#7511) *)
type 'a s = S : 'a -> 'a option s [@@unboxed];;
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a option s [@@unboxed]
type t = T : 'a s -> t [@@unboxed]
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
type t = T : (unit -> 'a) M.r -> t [@@unboxed]
|}];;

type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed];;
[%%expect{|
type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed]
|}];;

(* reject *)
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
type t = T : 'a s -> t [@@unboxed]
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

(* regression test for PR#7511 (wrong determination of unboxability for GADTs)
*)
type 'a s = S : 'a -> 'a s [@@unboxed];;
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a s [@@unboxed]
type t = T : 'a s -> t [@@unboxed]
|}];;

(* regression test for GPR#1133 (follow-up to PR#7511) *)
type 'a s = S : 'a -> 'a option s [@@unboxed];;
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
type 'a s = S : 'a -> 'a option s [@@unboxed]
type t = T : 'a s -> t [@@unboxed]
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
type t = T : (unit -> 'a) M.r -> t [@@unboxed]
|}];;

type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed];;
[%%expect{|
type 'a s = S : (unit -> 'a) M.r -> 'a option s [@@unboxed]
|}];;

(* reject *)
type t = T : _ s -> t [@@unboxed];;
[%%expect{|
type t = T : 'a s -> t [@@unboxed]
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
