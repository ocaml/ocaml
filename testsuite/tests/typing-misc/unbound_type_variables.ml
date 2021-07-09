(* TEST
   * expect
*)

type synonym = 'a -> 'a

[%%expect{|
Line 1, characters 15-17:
1 | type synonym = 'a -> 'a
                   ^^
Error: The type variable 'a is unbound in this type declaration.
|}]

type record = { contents: 'a }

[%%expect{|
Line 1, characters 26-28:
1 | type record = { contents: 'a }
                              ^^
Error: The type variable 'a is unbound in this type declaration.
|}]

type wrapper = Wrapper of 'a

[%%expect{|
Line 1, characters 26-28:
1 | type wrapper = Wrapper of 'a
                              ^^
Error: The type variable 'a is unbound in this type declaration.
|}]

(* This type secretly has a type variable in it *)
type polyvariant = [> `C]

[%%expect{|
Line 1, characters 0-25:
1 | type polyvariant = [> `C]
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type [> `C ] as 'a the variable 'a is unbound
|}]

type 'a only_one = 'a * 'b

[%%expect{|
Line 1, characters 24-26:
1 | type 'a only_one = 'a * 'b
                            ^^
Error: The type variable 'b is unbound in this type declaration.
|}]

type extensible = ..
type extensible += Extension of 'a

[%%expect{|
type extensible = ..
Line 2, characters 32-34:
2 | type extensible += Extension of 'a
                                    ^^
Error: The type variable 'a is unbound in this type declaration.
|}]
