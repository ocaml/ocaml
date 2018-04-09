(* TEST
   * expect
*)

let not_a_function = 1 + not_a_function

[%%expect{|
Line _, characters 25-39:
  let not_a_function = 1 + not_a_function
                           ^^^^^^^^^^^^^^
Error: Unbound value not_a_function
|}]
