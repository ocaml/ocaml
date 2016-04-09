module F (X : sig end) = struct type t = int end;;
type t = F(Does_not_exist).t;;
