(* TEST
   * expect
*)

class type virtual ['a] c = object constraint 'a = [<`A of int & float] end
[%%expect {|
Line 1, characters 0-75:
1 | class type virtual ['a] c = object constraint 'a = [<`A of int & float] end
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type of this class,
       class virtual ['a] c :
         object constraint 'a = _[< `A of int & float ] end,
       contains non-collapsible conjunctive types in constraints.
       Type int is not compatible with type float
|}]

class type ct = object
  method x : int
end

class c (y : 'a * float) : ct = object
  method x = y
end
[%%expect{|
class type ct = object method x : int end
Lines 5-7, characters 32-3:
5 | ................................object
6 |   method x = y
7 | end
Error: The class type object method x : 'a * float end
       is not matched by the class type ct
       The class type object method x : 'a * float end
       is not matched by the class type object method x : int end
       The method x has type 'a * float but is expected to have type int
       Type 'a * float is not compatible with type int
|}]

let foo = 42#m;;
[%%expect{|
Line 1, characters 10-12:
1 | let foo = 42#m;;
              ^^
Error: This expression is not an object; it has type int
|}]

let foo = object (self) method foo = self#bar end;;
[%%expect{|
Line 1, characters 37-41:
1 | let foo = object (self) method foo = self#bar end;;
                                         ^^^^
Error: This expression has no method bar
|}]
