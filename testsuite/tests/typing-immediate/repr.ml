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
