(* TEST
   expect;
*)

external identity : 'a -> 'a = "%identity"

[%%expect {|
external identity : 'a -> 'a = "%identity"
|}]

(* We use regular unification to check whether the requisite type equalities are
   possible, sometimes rejecting signatures in cases where it is possible to
   define a satisfactory structure. *)
module Int_backed : sig
  type t
  external to_int : t -> int = identity
end = struct
  type t = int
  external to_int : t -> int = identity
end

[%%expect {|
Line 3, characters 20-28:
3 |   external to_int : t -> int = identity
                        ^^^^^^^^
Error: The type of this alias does not match that of the aliased primitive.
       Type "t -> int" is not compatible with type "'a -> 'a"
       Type "int" is not compatible with type "'a" = "t"
|}]
