(* TEST
    flags = "-dsource"
    * expect
*)

module type S = sig
  type t
  type t' := t
end
[%%expect {|

module type S  = sig type t type t' := t end;;
module type S = sig type t end
|}]
