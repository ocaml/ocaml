(* TEST
   flags = "-strict-sequence"
   * expect
*)

if 3 then ();;

[%%expect{|
Line _, characters 3-4:
  if 3 then ();;
     ^
Error: This expression has type int but an expression was expected of type
         bool
       because it is in the condition of an if-statement
|}];;

fun b -> if true then (print_int b) else (if b then ());;

[%%expect{|
Line _, characters 45-46:
  fun b -> if true then (print_int b) else (if b then ());;
                                               ^
Error: This expression has type int but an expression was expected of type
         bool
       because it is in the condition of an if-statement
|}];;

(* Left-to-right bias is still there: if we swap the branches, the new error
   message does not show up because of propagation order. *)
fun b -> if true then (if b then ()) else (print_int b);;

[%%expect{|
Line _, characters 53-54:
  fun b -> if true then (if b then ()) else (print_int b);;
                                                       ^
Error: This expression has type bool but an expression was expected of type
         int
|}];;

if (let x = 3 in x) then ();;

[%%expect{|
Line _, characters 17-18:
  if (let x = 3 in x) then ();;
                   ^
Error: This expression has type int but an expression was expected of type
         bool
       because it is in the condition of an if-statement
|}];;

if (if true then 3 else 4) then ();;

[%%expect{|
Line _, characters 17-18:
  if (if true then 3 else 4) then ();;
                   ^
Error: This expression has type int but an expression was expected of type
         bool
       because it is in the condition of an if-statement
|}];;

if true then 3;;

[%%expect{|
Line _, characters 13-14:
  if true then 3;;
               ^
Error: This expression has type int but an expression was expected of type
         unit
       because it is in the result of a conditional with no else branch
|}];;

if (fun x -> x) then ();;

[%%expect{|
Line _, characters 3-15:
  if (fun x -> x) then ();;
     ^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is
bool
because it is in the condition of an if-statement
|}];;

while 42 do () done;;

[%%expect{|
Line _, characters 6-8:
  while 42 do () done;;
        ^^
Error: This expression has type int but an expression was expected of type
         bool
       because it is in the condition of a while-loop
|}];;

(* -strict-sequence is required for this test to fail, otherwise only a warning
   is produced *)
while true do (if true then 3 else 4) done;;

[%%expect{|
Line _, characters 14-37:
  while true do (if true then 3 else 4) done;;
                ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type int but an expression was expected of type
         unit
       because it is in the body of a while-loop
|}];;

for i = 3. to 4 do () done;;

[%%expect{|
Line _, characters 8-10:
  for i = 3. to 4 do () done;;
          ^^
Error: This expression has type float but an expression was expected of type
         int
       because it is in a for-loop start index
|}];;

for i = 3 to 4. do () done;;

[%%expect{|
Line _, characters 13-15:
  for i = 3 to 4. do () done;;
               ^^
Error: This expression has type float but an expression was expected of type
         int
       because it is in a for-loop stop index
|}];;

(* -strict-sequence is required for this test to fail, otherwise only a warning
   is produced *)
for i = 0 to 0 do (if true then 3 else 4) done;;

[%%expect{|
Line _, characters 18-41:
  for i = 0 to 0 do (if true then 3 else 4) done;;
                    ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type int but an expression was expected of type
         unit
       because it is in the body of a for-loop
|}];;

assert 12;;

[%%expect{|
Line _, characters 7-9:
  assert 12;;
         ^^
Error: This expression has type int but an expression was expected of type
         bool
       because it is in the condition of an assertion
|}];;

(* -strict-sequence is also required for this test to fail *)
(let x = 3 in x+1); ();;

[%%expect{|
Line _, characters 0-18:
  (let x = 3 in x+1); ();;
  ^^^^^^^^^^^^^^^^^^
Error: This expression has type int but an expression was expected of type
         unit
       because it is in the left-hand side of a sequence
|}];;

let ordered_list_with x y =
  if x <= y then [x;y]
  else if x > y then [y;x]

[%%expect{|
Line _, characters 22-26:
    else if x > y then [y;x]
                        ^^^^
Error: This variant expression is expected to have type unit
         because it is in the result of a conditional with no else branch
       The constructor :: does not belong to type unit
|}];;
