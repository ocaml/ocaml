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
