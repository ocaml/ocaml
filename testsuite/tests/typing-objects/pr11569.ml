(* TEST
   * expect
*)

class ['a] c = object constraint 'a = int method m (x: bool #c) = () end;;

[%%expect{|
Line 1, characters 0-72:
1 | class ['a] c = object constraint 'a = int method m (x: bool #c) = () end;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The class type #c is used with parameter(s) bool ,
       whereas the class type definition constrains those parameters to be
       int
|}]

class ['a, 'b] c = object constraint 'a = int method m (x: (bool, 'b) #c) = () end;;

[%%expect{|
Line 1, characters 0-82:
1 | class ['a, 'b] c = object constraint 'a = int method m (x: (bool, 'b) #c) = () end;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The class type #c is used with parameter(s) (bool, 'b) ,
       whereas the class type definition constrains those parameters to be
       (int, 'b)
|}]

class c = object method m (x: #c) = int_of_string x#m end;;

[%%expect{|
Line 1, characters 0-57:
1 | class c = object method m (x: #c) = int_of_string x#m end;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The abbreviation #c expands to type < m : 'a -> int; .. >
       but is used with type < m : string; .. > as 'a
|}]
