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

type x' = private T

[%%expect{|
type x' = private T
|}]

type a' = x' = T

[%%expect{|
Line 1, characters 0-16:
1 | type a' = x' = T
    ^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "x'"
       Private variant constructor(s) would be revealed.
|}]

type y = x'
type b = y = T

[%%expect{|
type y = x'
Line 2, characters 0-14:
2 | type b = y = T
    ^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "y"
       Private variant constructor(s) would be revealed.
|}]
