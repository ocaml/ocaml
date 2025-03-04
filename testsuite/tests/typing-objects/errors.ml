(* TEST
 expect;
*)

class type virtual ['a] c = object constraint 'a = [<`A of int & float] end
[%%expect {|
Line 1, characters 0-75:
1 | class type virtual ['a] c = object constraint 'a = [<`A of int & float] end
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type of this class,
       "class virtual ['_a] c :
         object constraint '_a = [< `A of int & float ] as '_weak1 end",
       contains non-collapsible conjunctive types in constraints.
       Type "int" is not compatible with type "float"
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
       The method x has type "'a * float" but is expected to have type "int"
       Type "'a * float" is not compatible with type "int"
|}]

let foo = 42#m;;
[%%expect{|
Line 1, characters 10-12:
1 | let foo = 42#m;;
              ^^
Error: This expression is not an object; it has type "int"
|}]

let foo = object (self) method foo = self#bar end;;
[%%expect{|
Line 1, characters 37-41:
1 | let foo = object (self) method foo = self#bar end;;
                                         ^^^^
Error: This expression has no method "bar"
|}]

class empty = object end
class also_empty = object inherit! empty end
[%%expect{|
class empty : object  end
Line 2, characters 26-40:
2 | class also_empty = object inherit! empty end
                              ^^^^^^^^^^^^^^
Error: This inheritance does not override any methods or instance variables
       but is explicitly marked as overriding with "!".
|}]


class ['a] c = object val x : 'a list ref = ref [] end
class ['a] x = let r = ref [] in object val x : 'a list ref = r end
[%%expect{|
class ['a] c : object val x : 'a list ref end
Line 2, characters 0-67:
2 | class ['a] x = let r = ref [] in object val x : 'a list ref = r end
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type of this class,
       "class ['_a] x : object val x : '_a list ref end",
       contains the non-generalizable type variable(s): "'_a".
       (see manual section 6.1.2)
|}]
