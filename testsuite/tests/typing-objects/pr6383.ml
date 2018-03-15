(* TEST
   * expect
*)

let f (x: #M.foo) = 0;;
[%%expect{|
Line _, characters 11-16:
  let f (x: #M.foo) = 0;;
             ^^^^^
Error: Unbound module M
|}];;
