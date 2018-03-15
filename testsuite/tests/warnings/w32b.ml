
(* Check that [t] is considered unused without an .mli file (see GPR#1358) *)
module Q (M : sig type t end) = struct end
