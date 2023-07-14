(* TEST_BELOW *)

module A = struct type r type s type t end;;

(* Local open is not allowed in places that could look like either
   several arguments or one argument in a constructor declaration. *)

type this_is_rejected = Foo of A.(r * s * t);;

type as_a_consquence_this_is_also_rejected = Foo of A.(r);;

(* To make the grammar more regular, we reject local opens
   in all forms of constructor arguments, including multi-arguments. *)

type rejected_for_consistency = Foo of A.(r) * A.(s);;


(* It is possible to explicitly disambiguate that you want
   a single argument using extra parentheses: *)
type this_is_accepted = Foo of (A.(r * s * t));;

(* But currently there is no syntax to explicitly open
   around several arguments *)
type this_is_not_supported = A.(Foo of r * s * t);;

(* TEST
 toplevel;
*)
