(* TEST_BELOW
(* Blank lines added here to preserve locations. *)
*)

module type S = struct end
;;

module type S = sig
  module type S = struct end
end
;;

module type S = sig
  module M : struct end
end
;;

module type S = sig
  module M = struct end
end
;;

module M : sig end
;;

module M : struct end
;;

module type S = sig
  module M : functor (X : S) -> struct end
end
;;

module M = X.Y(sig end)
;;

module type S = sig
  include module type of sig end
end
;;

(* TEST
 toplevel;
*)
