(* TEST
   * expect
*)

let facto n =   (* missing [rec] *)
   if n = 0 then 1 else n * facto (n-1)

[%%expect{|
Line _, characters 28-33:
Error: Unbound value facto.
       Hint: You are probably missing the `rec' keyword on line 1.
|}]
