
module M: sig
  val odd:  int -> unit -> int -> unit -> int -> unit
  val even: unit -> int -> unit -> int -> unit -> int
  val t_3: unit -> unit -> int -> unit -> unit -> int -> unit -> unit -> int
  val t_2: unit -> int -> unit -> unit -> int -> unit -> unit -> int -> unit
  val t_1: int -> unit -> unit -> int -> unit -> unit -> int -> unit -> unit
end = struct

  let odd a b c d e = a + b + c + d + e

  let t_3 a b c d e f g h = a + b + c + d + e + f + g + h

  let t_1 a b c d e f g h = a + b + c + d + e + f + g + h

end;;
[%%expect {|
Line _, characters 6-176:
Error: Signature mismatch:
       Modules do not match:
         sig ... * 3 end
       is not included in
         sig ... !(val even : ... * 5 -> int) ... * 3 end
       The value `t_2' is required but not provided
       The value `even' is required but not provided
|}]


module N: sig
 type inferretque = A type deos = B type latio = C type genus = D
 type unde = E type latinum = F type albanique = G type patres = H
 type atque = I type altae = J type moenia = K
end = struct
 type inferretque = A type deos = X type latio = C type genus = X
 type unde = E type latinum = F type albanique = X type patres = X
 type atque = I type altae = J type moenia = X
end;;
[%%expect {|
Line _, characters 6-196:
Error: Signature mismatch:
       Modules do not match:
         sig ... type deos = !(X) ... * 4 type albanique = !(X) ... * 4 end
       is not included in
         sig ... type deos = !(B) ... * 4 type albanique = !(G) ... * 4 end
       Type declarations do not match:
         type deos = !(X)
       is not included in
         type deos = !(B)
       Fields number 1 have different names, X and B.
|}]

module Absurd: sig val x:'a end = struct let x = 0 end;;
[%%expect {|
Line _, characters 34-54:
Error: Signature mismatch:
       Modules do not match:
         sig val x : !(int) end
       is not included in
         sig val x : !('a) end
       Values do not match: val x : !(int) is not included in val x : !('a)
|}]
