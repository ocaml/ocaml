(* TEST
 expect;
*)

type x=T
type a=x=T
type y=x
type b=y=T

[%%expect{|
type x = T
type a = x = T
type y = x
type b = y = T
|}]

module rec Q : sig
  type x = private T
end = Q

[%%expect{|
module rec Q : sig type x = private T end
|}]

type a = Q.x = T

[%%expect{|
Line 1, characters 0-16:
1 | type a = Q.x = T
    ^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "Q.x"
       Private variant constructor(s) would be revealed.
|}]

type y = Q.x
type b = y = T

[%%expect{|
type y = Q.x
Line 2, characters 0-14:
2 | type b = y = T
    ^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "y"
       Private variant constructor(s) would be revealed.
|}]
