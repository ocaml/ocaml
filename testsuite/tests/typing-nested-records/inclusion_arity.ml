(* TEST
 expect;
*)

(* Regression test: inclusion between nested record declarations whose
   parameter lists have different lengths must report a normal mismatch,
   not raise a fatal [Invalid_argument]. *)

module type S = sig
  type ('a, 'b) t = { n : { value : 'a } } constraint 'a = 'b
end

module M : S = struct
  type ('a, 'b) t = { n : { value : ('b as 'a) } }
end
[%%expect{|
module type S =
  sig type ('b, 'a) t = { n : { value : 'b; }; } constraint 'a = 'b end
Lines 5-7, characters 15-3:
5 | ...............struct
6 |   type ('a, 'b) t = { n : { value : ('b as 'a) } }
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a, 'b) t = { n : { value : 'a; }; } constraint 'b = 'a
         end
       is not included in
         S
       Type declarations do not match:
         type ('a, 'b) t = { n : { value : 'a; }; } constraint 'b = 'a
       is not included in
         type ('b, 'a) t = { n : { value : 'b; }; } constraint 'a = 'b
       Fields do not match:
         "n : { value : 'a; };"
       is not the same as:
         "n : { value : 'b; };"
       Their nested record definitions differ.
|}]

(* Reverse inclusion direction: the constraint form is checked against
   the alias form, which reaches the comparison in the other order. *)

module type S2 = sig
  type ('a, 'b) t = { n : { value : ('b as 'a) } }
end

module M2 : S2 = struct
  type ('a, 'b) t = { n : { value : 'a } } constraint 'a = 'b
end
[%%expect{|
module type S2 =
  sig type ('a, 'b) t = { n : { value : 'a; }; } constraint 'b = 'a end
Lines 5-7, characters 17-3:
5 | .................struct
6 |   type ('a, 'b) t = { n : { value : 'a } } constraint 'a = 'b
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('b, 'a) t = { n : { value : 'b; }; } constraint 'a = 'b
         end
       is not included in
         S2
       Type declarations do not match:
         type ('b, 'a) t = { n : { value : 'b; }; } constraint 'a = 'b
       is not included in
         type ('a, 'b) t = { n : { value : 'a; }; } constraint 'b = 'a
       Fields do not match:
         "n : { value : 'b; };"
       is not the same as:
         "n : { value : 'a; };"
       Their nested record definitions differ.
|}]

(* Equal-arity nested declarations must keep matching. *)

module type S3 = sig
  type 'a t = { n : { value : 'a } }
end

module M3 : S3 = struct
  type 'a t = { n : { value : 'a } }
end
[%%expect{|
module type S3 = sig type 'a t = { n : { value : 'a; }; } end
module M3 : S3
|}]
