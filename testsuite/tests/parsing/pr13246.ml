(* TEST
  expect;
*)

module M = struct
  type t = true | false
  let \#true = 13
end

let not = function
  | M.true -> M.false
  | M.false -> M.true

let t2 = M.true
let t3 = M.\#true

[%%expect{|
module M : sig type t = true | false val \#true : int end
val not : M.t -> M.t = <fun>
val t2 : M.t = M.true
val t3 : int = 13
|}]
