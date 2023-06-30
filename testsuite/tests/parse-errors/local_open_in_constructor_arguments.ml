(* TEST_BELOW *)

module A = struct type r type s type t end;;

(* Local open is not allowed in places that could look like either
   several arguments or one argument in a constructor declaration. *)

type this_is_accepted = Foo of (A.(r * s * t));;

type this_is_rejected = Foo of A.(r * s * t);;

(* currently this is not supported: A.(Foo of r * s * t) *)

(* TEST
 toplevel;
*)
