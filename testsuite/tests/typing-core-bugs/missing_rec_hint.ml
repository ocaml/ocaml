(* TEST
   * expect
*)

let facto n =   (* missing [rec] *)
   if n = 0 then 1 else n * facto (n-1)

[%%expect{|
Line 2, characters 28-33:
2 |    if n = 0 then 1 else n * facto (n-1)
                                ^^^^^
Error: Unbound value facto
Hint: If this is a recursive definition,
you should add the 'rec' keyword on line 1
|}];;

let x = 3 in
let f x = f x in
()

[%%expect{|
Line 2, characters 10-11:
2 | let f x = f x in
              ^
Error: Unbound value f
Hint: If this is a recursive definition,
you should add the 'rec' keyword on line 2
|}];;

let f x = if x < 0 then x else h (x-1)
and g x = if x < 0 then x else f (x-1)
and h x = if x < 0 then x else g (x-1)

[%%expect{|
Line 1, characters 31-32:
1 | let f x = if x < 0 then x else h (x-1)
                                   ^
Error: Unbound value h
Hint: If this is a recursive definition,
you should add the 'rec' keyword on line 1
|}];;

let value1 = 3 in
let value2 = value2 (* typo: should be value1 *) + 1 in
()

[%%expect{|
Line 2, characters 13-19:
2 | let value2 = value2 (* typo: should be value1 *) + 1 in
                 ^^^^^^
Error: Unbound value value2
Hint: Did you mean value1?
|}];;

let foobar1 () = () in
let foobar2 () = foobar2 () (* typo? or missing "rec"? *) in
()

[%%expect{|
Line 2, characters 17-24:
2 | let foobar2 () = foobar2 () (* typo? or missing "rec"? *) in
                     ^^^^^^^
Error: Unbound value foobar2
Hint: Did you mean foobar1?
Hint: If this is a recursive definition,
you should add the 'rec' keyword on line 2
|}];;
