(* TEST
   * toplevel
*)

module type Rejected1 = sig
  type t1 := A
end;;

module type Rejected2 = sig
  type t2 := { x : int }
end;;

module type RejectedM1 = sig
  module M1 := sig end
end;;

module F(X : sig type t end) = struct
  type t = X.t
end;;

module type RejectedM2 = sig
  module M2 := F(struct type t = int end)
end;;

type t := int;;

module M := List;;

module type Rejected3 = sig
  type t3 := int
  and u3 = char
end;;

module type Rejected0 = sig
  type nonrec t := int
end;;
