(* TEST
   * expect
*)

type i = int

type 'a t = T : i
[%%expect{|
type i = int
Line 3, characters 16-17:
3 | type 'a t = T : i
                    ^
Error: Constraints are not satisfied in this type.
       Type i should be an instance of 'a t
|}]

type 'a t = T : i t
type 'a s = 'a t = T : i t
[%%expect{|
type 'a t = T : i t
Line 2, characters 23-26:
2 | type 'a s = 'a t = T : i t
                           ^^^
Error: Constraints are not satisfied in this type.
       Type i t should be an instance of 'a s
|}]

type 'a t = T : i s
and  'a s = 'a t
[%%expect{|
Line 1, characters 16-19:
1 | type 'a t = T : i s
                    ^^^
Error: Constraints are not satisfied in this type.
       Type i s should be an instance of 'a t
|}]
