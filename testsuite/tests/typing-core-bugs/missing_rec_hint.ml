(* TEST
   * expect
*)

let facto n =   (* missing [rec] *)
   if n = 0 then 1 else n * facto (n-1)

[%%expect{|
Line _, characters 28-33:
     if n = 0 then 1 else n * facto (n-1)
                              ^^^^^
Error: Unbound value facto.
       Hint: You are probably missing the `rec' keyword on line 1.
|}];;

let x = 3 in
let f x = f x in
()

[%%expect{|
Line _, characters 10-11:
  let f x = f x in
            ^
Error: Unbound value f.
       Hint: You are probably missing the `rec' keyword on line 2.
|}];;

let f x = if x < 0 then x else h (x-1)
and g x = if x < 0 then x else f (x-1)
and h x = if x < 0 then x else g (x-1)

[%%expect{|
Line _, characters 31-32:
  let f x = if x < 0 then x else h (x-1)
                                 ^
Error: Unbound value h.
       Hint: You are probably missing the `rec' keyword on line 1.
|}];;
