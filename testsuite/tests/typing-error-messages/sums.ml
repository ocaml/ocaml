
module N: sig
        type c = A | B | C | D | E | F | G
end = struct
        type c = A | B | C | G | H | I | J
end;;
[%%expect {|
Line _, characters 6-59:
Error: Signature mismatch:
       Modules do not match:
         sig type c = ... * 3 | !(G) | !(H) | ... * 2 end
       is not included in
         sig type c = ... * 3 | !(D) | !(E) | ... * 2 end
       Type declarations do not match:
         type c = ... * 3 | !(G) | !(H) | ... * 2
       is not included in
         type c = ... * 3 | !(D) | !(E) | ... * 2
       Fields number 4 have different names, G and D.
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


module M: sig
  type t = A | B | C | D | E | F end = struct
  type t = A | B | C | D | E | G end;;
[%%expect {|
Line _, characters 39-82:
Error: Signature mismatch:
       Modules do not match:
         sig type t = ... * 5 | !(G) end
       is not included in
         sig type t = ... * 5 | !(F) end
       Type declarations do not match:
         type t = ... * 5 | !(G)
       is not included in
         type t = ... * 5 | !(F)
       Fields number 6 have different names, G and F.
|}]


module M: sig
type t = ..
type t += A | B | A2
type t += C0 | C | C2 | D
type t += E | F
end = struct
type t = ..
type t += A | A2 | B | C0 | C | C2 | E | F
end;;
[%%expect {|
Line _, characters 6-71:
Error: Signature mismatch:
       Modules do not match:
         sig ... * 9 end
       is not included in
         sig ... * 7 !(type t += D) ... * 2 end
       The extension constructor `D' is required but not provided
|}]
