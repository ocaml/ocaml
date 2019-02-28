(* TEST
   * expect
*)

module X = struct

  module B = List

  exception B of {x:int}
end

let _ = X.B {x=2}
;;
[%%expect{|
module X : sig module B = List exception B of { x : int; } end
- : exn = X.B {x = 2}
|}]
