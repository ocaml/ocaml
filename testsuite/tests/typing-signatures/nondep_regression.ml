(* TEST
 expect;
*)

type 'a seq = 'a list

module Make (A : sig type t end) = struct
  type t = A.t seq
end

module H = Make (struct type t end)

[%%expect{|
type 'a seq = 'a list
module Make : (A : sig type t end) -> sig type t = A.t seq end
module H : sig type t end
|}]
