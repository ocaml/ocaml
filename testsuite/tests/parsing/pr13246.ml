(* TEST
  expect;
*)

module M = struct type t = true | false end

let not = function
  | M.true -> M.false
  | M.false -> M.true

[%%expect{|
module M : sig type t = true | false end
val not : M.t -> M.t = <fun>
|}]
