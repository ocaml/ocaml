module type S = sig type t [@@repr(address)] end;;
module F (M : S) : S = M;;
[%%expect{|
module type S = sig type t [@@repr(address)] end
module F : functor (M : S) -> S
|}];;

(* VALID DECLARATIONS *)

module A = struct
  (* Abstract types can be addresses *)
  type t [@@repr(address)]

  (* [@@repr(address)] tag here is unnecessary but valid since t has it *)
  type s = t [@@repr(address)]

  (* Again, valid alias even without tag *)
  type r = s

  (** Variant types *)
  type w = Foo | Bar of int [@@repr(address)]

  (* Mutually recursive declarations work as well *)
  type p = q [@@repr(address)]
  and q = T of int

  (* Check subtyping: integers are also addresses *)
  type u = int [@@repr(address)]
end;;
[%%expect{|
module A :
  sig
    type t [@@repr(address)]
    type s = t [@@repr(address)]
    type r = s
    type w = Foo | Bar of int [@@repr(address)]
    type p = q [@@repr(address)]
    and q = T of int
    type u = int [@@repr(address)]
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
  type t = float [@@repr(address)]
end;;
[%%expect{|
Line _, characters 2-34:
Error: Types marked with the address attribute must be
       any type wich is neither lazy nor float
|}];;

(* Not guaranteed that t is address, so this is an invalid declaration *)
module C = struct
  type t
  type s = t [@@repr(address)]
end;;
[%%expect{|
Line _, characters 2-30:
Error: Types marked with the address attribute must be
       any type wich is neither lazy nor float
|}];;

(* Can't ascribe to an address type signature with a non-address type *)
module D : sig type t [@@repr(address)] end = struct
  type t = float
end;;
[%%expect{|
Line _, characters 46-73:
Error: Signature mismatch:
       Modules do not match:
         sig type t = float end
       is not included in
         sig type t [@@repr(address)] end
       Type declarations do not match:
         type t = float
       is not included in
         type t [@@repr(address)]
       Their internal representations differ:
       the type representation cannot be "address".
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
         type t [@@repr(address)]
       Their internal representations differ:
       the type representation cannot be "address".
|}];;

(* Can't use a non-address type even if mutually recursive *)
module E = struct
  type t = s [@@repr(address)]
  and s = float
end;;
[%%expect{|
Line _, characters 2-30:
Error: Types marked with the address attribute must be
       any type wich is neither lazy nor float
|}];;
