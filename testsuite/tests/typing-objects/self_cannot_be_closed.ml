(* TEST
 expect;
*)
let is_empty (x : < >) = ();;
[%%expect {|
val is_empty : <  > -> unit = <fun>
|}]

class c = object (self) method private foo = is_empty self end;;
[%%expect {|
Line 1, characters 54-58:
1 | class c = object (self) method private foo = is_empty self end;;
                                                          ^^^^
Error: The value "self" has type "< .. >" but an expression was expected of type
         "<  >"
       Self type cannot be unified with a closed object type
|}]
