module type S = sig type t [@@addr] end;;
module F (M : S) : S = M;;
[%%expect{|
module type S = sig type t [@@addr] end
module F : functor (M : S) -> S
|}];;

(* VALID DECLARATIONS *)

module A = struct
  (* Abstract types can be addresses *)
  type t [@@addr]

  (* [@@addr] tag here is unnecessary but valid since t has it *)
  type s = t [@@addr]

  (* Again, valid alias even without tag *)
  type r = s

  (** Variant types *)
  type w = Foo | Bar of int [@@addr]

  (* Mutually recursive declarations work as well *)
  type p = q [@@addr]
  and q = T of int

  (* Check subtyping: integers are also addresses *)
  type u = int [@@addr]
end;;
[%%expect{|
module A :
  sig
    type t [@@addr]
    type s = t
    type r = s
    type w = Foo | Bar of int
    type p = q
    and q = T of int
    type u = int
  end
|}];;

(* Valid using an explicit signature *)
module M_valid : S = struct type t = T of int end;;
module FM_valid = F (struct type t = T of int end);;
[%%expect{|
module M_valid : S
module FM_valid : S
|}];;

(* INVALID DECLARATIONS *)

(* Cannot directly declare a non-address type as address *)
module B = struct
  type t = float [@@addr]
end;;
[%%expect{|
Line _, characters 2-25:
Error: Explicit representation attribute (addr) not compatible with inferred representation (float)
|}];;

(* Not guaranteed that t is address, so this is an invalid declaration *)
module C = struct
  type t
  type s = t [@@addr]
end;;
[%%expect{|
Line _, characters 2-21:
Error: Explicit representation attribute (addr) not compatible with inferred representation (generic)
|}];;

(* Can't ascribe to an address type signature with a non-address type *)
module D : sig type t [@@addr] end = struct
  type t = float
end;;
[%%expect{|
Line _, characters 37-64:
Error: Signature mismatch:
       Modules do not match:
         sig type t = float end
       is not included in
         sig type t [@@addr] end
       Type declarations do not match:
         type t = float
       is not included in
         type t [@@addr]
       Their runtime representations do not agree.
|}];;

(* Same as above but with explicit signature *)
module M_invalid : S = struct type t = float end;;
[%%expect{|
Line _, characters 23-48:
Error: Signature mismatch:
       Modules do not match: sig type t = float end is not included in S
       Type declarations do not match:
         type t = float
       is not included in
         type t [@@addr]
       Their runtime representations do not agree.
|}];;
module FM_invalid = F (struct type t = float end);;
[%%expect{|
Line _, characters 23-48:
Error: Signature mismatch:
       Modules do not match: sig type t = float end is not included in S
       Type declarations do not match:
         type t = float
       is not included in
         type t [@@addr]
       Their runtime representations do not agree.
|}];;

(* Can't use a non-address type even if mutually recursive *)
module E = struct
  type t = s [@@addr]
  and s = float
end;;
[%%expect{|
Line _, characters 2-21:
Error: Explicit representation attribute (addr) not compatible with inferred representation (float)
|}];;


(* Representation sees through unboxed types *)

module M_valid: sig
  type t [@@float]
end = struct
  type t = A of float [@@float] [@@unboxed]
end;;
[%%expect{|
module M_valid : sig type t [@@float] end
|}];;

(* All-float record representation *)

module M_valid : sig
  type t [@@float]
  type r = {x:t; y: t}
end = struct
  type t = float
  type r = {x:t; y: t}
end;;
[%%expect{|
module M_valid : sig type t [@@float] type r = { x : t; y : t; } end
|}];;

module M_valid : sig
  type t [@@float]
  and s = A of t [@@unboxed]
  and r = {x:s; y: s}
end = struct
  type t = float
  and s = A of t [@@unboxed]
  and r = {x:s; y: s}
end;;
[%%expect{|
module M_valid :
  sig
    type t [@@float]
    and s = A of t [@@unboxed]
    and r = { x : s; y : s; }
  end
|}];;

module M_invalid : sig
  type t
  type r = {x:t; y: t}
end = struct
  type t = float
  type r = {x:t; y: t}
end;;
[%%expect{|
Line _, characters 6-56:
Error: Signature mismatch:
       Modules do not match:
         sig type t = float type r = { x : t; y : t; } end
       is not included in
         sig type t type r = { x : t; y : t; } end
       Type declarations do not match:
         type r = { x : t; y : t; }
       is not included in
         type r = { x : t; y : t; }
       Their internal representations differ:
       the first declaration uses unboxed float representation.
|}];;


(* (lazy x) does not need to be boxed for "addr" *)

module X : sig type t val x: t end = struct
  type t = A | B
  let x = A
end;;
(Obj.repr (lazy X.x) == Obj.repr X.x);;
[%%expect{|
module X : sig type t val x : t end
- : bool = false
|}];;

module X : sig type t [@@addr] val x: t end = struct
  type t = A | B
  let x = A
end;;
(Obj.repr (lazy X.x) == Obj.repr X.x);;
[%%expect{|
module X : sig type t [@@addr] val x : t end
- : bool = true
|}];;


(* Check interaction with unboxing GADTs *)

type t = A : ('a * ('a -> unit)) Lazy.t -> t [@@unboxed]
[%%expect{|
type t = A : ('a * ('a -> unit)) Lazy.t -> t [@@unboxed]
|}];;

module X : sig
  type 'a t [@@non_float]
end = struct
  type 'a t = { x : 'a }
end;;
type t = A : 'a X.t -> t [@@unboxed]
;;
[%%expect{|
module X : sig type 'a t [@@non_float] end
type t = A : 'a X.t -> t [@@unboxed]
|}];;

module X : sig
  type 'a t
end = struct
  type 'a t = { x : 'a }
end;;
type t = A : 'a X.t -> t [@@unboxed]
;;
[%%expect{|
module X : sig type 'a t end
Line _, characters 0-36:
Error: This type cannot be unboxed because
       it might contain both float and non-float values.
       You should annotate it with [@@ocaml.boxed].
|}];;

module F(X : sig type 'a t [@@non_contractive] end) = struct
  type t = A: 'a X.t -> t [@@unboxed]
end;;
[%%expect{|
module F :
  functor (X : sig type 'a t [@@non_contractive] end) ->
    sig type t = A : 'a X.t -> t [@@unboxed] end
|}];;

module M_valid = F(struct type 'a t = float end);;
[%%expect{|
module M_valid : sig type t = A : float -> t [@@unboxed] end
|}];;

module M_valid = F(struct type 'a t = A of float  [@@unboxed] end);;
[%%expect{|
module M_valid : sig type t [@@non_contractive] [@@unboxed] end
|}];;

module M_valid = F(struct type 'a t = int end);;
[%%expect{|
module M_valid : sig type t = A : int -> t [@@unboxed] end
|}];;

module M_invalid = F(struct type 'a t = 'a end);;
[%%expect{|
Line _, characters 21-46:
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a end
       is not included in
         sig type 'a t [@@non_contractive] end
       Type declarations do not match:
         type 'a t = 'a
       is not included in
         type 'a t [@@non_contractive]
       Their runtime representations do not agree.
|}];;

module M_invalid = F(struct type 'a t = A of 'a [@@unboxed] end);;
[%%expect{|
Line _, characters 21-63:
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = A of 'a [@@unboxed] end
       is not included in
         sig type 'a t [@@non_contractive] end
       Type declarations do not match:
         type 'a t = A of 'a [@@unboxed]
       is not included in
         type 'a t [@@non_contractive]
       Their runtime representations do not agree.
|}];;

module M_invalid = F(struct type 'a t = 'b constraint 'a = 'b list end);;
[%%expect{|
Line _, characters 21-70:
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'b constraint 'a = 'b list end
       is not included in
         sig type 'a t [@@non_contractive] end
       Type declarations do not match:
         type 'a t = 'b constraint 'a = 'b list
       is not included in
         type 'a t [@@non_contractive]
       Their constraints differ.
|}];;
