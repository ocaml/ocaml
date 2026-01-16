(* TEST
 expect;
*)

let f (x: #M.foo) = 0;;
[%%expect{|
Line 1, characters 11-12:
1 | let f (x: #M.foo) = 0;;
               ^
Error: Unbound module "M"
|}];;
