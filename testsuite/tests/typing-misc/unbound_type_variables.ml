(* TEST
 expect;
*)

type synonym = 'a -> 'a

[%%expect{|
Line 1, characters 15-17:
1 | type synonym = 'a -> 'a
                   ^^
Error: The type variable "'a" is unbound in this type declaration.
|}]

type record = { contents: 'a }

[%%expect{|
Line 1, characters 26-28:
1 | type record = { contents: 'a }
                              ^^
Error: The type variable "'a" is unbound in this type declaration.
|}]

type wrapper = Wrapper of 'a

[%%expect{|
Line 1, characters 26-28:
1 | type wrapper = Wrapper of 'a
                              ^^
Error: The type variable "'a" is unbound in this type declaration.
|}]

(* This type secretly has a type variable in it *)
type polyvariant = [> `C]

[%%expect{|
Line 1, characters 0-25:
1 | type polyvariant = [> `C]
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "[> `C ] as 'a" the variable "'a" is unbound
|}]

type 'a only_one = 'a * 'b

[%%expect{|
Line 1, characters 24-26:
1 | type 'a only_one = 'a * 'b
                            ^^
Error: The type variable "'b" is unbound in this type declaration.
|}]

type extensible = ..
type extensible += Extension of 'a

[%%expect{|
type extensible = ..
Line 2, characters 32-34:
2 | type extensible += Extension of 'a
                                    ^^
Error: The type variable "'a" is unbound in this type declaration.
|}]

type 'a t = [> `A]
[%%expect {|
Line 1, characters 0-18:
1 | type 'a t = [> `A]
    ^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "[> `A ] as 'b" the variable "'b" is unbound
|}]

type ('a,'b,'c,'d,'e,'f) t =
  | A of 'x
  | B of 'y
constraint 'x = 'c * _
constraint 'y = 'd * _
[%%expect {|
Lines 1-5, characters 0-22:
1 | type ('a,'b,'c,'d,'e,'f) t =
2 |   | A of 'x
3 |   | B of 'y
4 | constraint 'x = 'c * _
5 | constraint 'y = 'd * _
Error: A type variable is unbound in this type declaration.
       In case "A of ('c * 'g)" the variable "'g" is unbound
|}]


type ('a,'b,'c,'d,'e,'f) t = { a: 'x; b: 'y }
constraint 'x = 'c * _
constraint 'y = 'd * _
[%%expect {|
Lines 1-3, characters 0-22:
1 | type ('a,'b,'c,'d,'e,'f) t = { a: 'x; b: 'y }
2 | constraint 'x = 'c * _
3 | constraint 'y = 'd * _
Error: A type variable is unbound in this type declaration.
       In field "a: 'c * 'g" the variable "'g" is unbound
|}]

type ('a,'b,'c,'d,'e,'f) t =
  | A of { r:'x; s:'y }
  | B of { t: 'z; w:'w}
constraint 'x = 'c * _
constraint 'y = 'd * _
constraint 'z = 'f * _
constraint 'w = _
[%%expect {|
Lines 1-7, characters 0-17:
1 | type ('a,'b,'c,'d,'e,'f) t =
2 |   | A of { r:'x; s:'y }
3 |   | B of { t: 'z; w:'w}
4 | constraint 'x = 'c * _
5 | constraint 'y = 'd * _
6 | constraint 'z = 'f * _
7 | constraint 'w = _
Error: A type variable is unbound in this type declaration.
       In case "A of { r : 'c * 'g; s : 'd * 'h; }" the variable "'g" is unbound
|}]

class ['a,'b,'c,'d,'e,'f] c = object
constraint 'x = 'a * _ * 'c
constraint 'y = 'd * _
  method m: 'x * 'y = assert false
end

[%%expect{|
Lines 1-5, characters 0-3:
1 | class ['a,'b,'c,'d,'e,'f] c = object
2 | constraint 'x = 'a * _ * 'c
3 | constraint 'y = 'd * _
4 |   method m: 'x * 'y = assert false
5 | end
Error: Some type variables are unbound in this type:
         class ['a, 'b, 'c, 'd, 'e, 'f] c :
           object method m : ('a * 'g * 'c) * ('d * 'h) end
       The method "m" has type "('a * 'g * 'c) * ('d * 'h)" where "'g" is unbound
|}]
