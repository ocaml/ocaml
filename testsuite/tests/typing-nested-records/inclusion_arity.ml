(* TEST
 expect;
*)

(* Canonical parameter selection gives both sides the same arity. The internal guard is tested in inclusion_arity_internal.ml. *)

module type S = sig
  type ('a, 'b) t = { n : { value : 'a } } constraint 'a = 'b
end

module M : S = struct
  type ('a, 'b) t = { n : { value : ('b as 'a) } }
end
[%%expect{|
module type S =
  sig type ('b, 'a) t = { n : { value : 'b; }; } constraint 'a = 'b end
module M : S
|}]

(* Check the reverse direction. *)

module type S2 = sig
  type ('a, 'b) t = { n : { value : ('b as 'a) } }
end

module M2 : S2 = struct
  type ('a, 'b) t = { n : { value : 'a } } constraint 'a = 'b
end
[%%expect{|
module type S2 =
  sig type ('a, 'b) t = { n : { value : 'a; }; } constraint 'b = 'a end
module M2 : S2
|}]

(* Equal-arity nested declarations keep matching. *)

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
