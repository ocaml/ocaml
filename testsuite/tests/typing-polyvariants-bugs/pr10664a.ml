(* TEST
   * expect
*)

class idfunc =
  object
    method id : 'ab. ([< `A | `B ] as 'ab) -> 'ab = fun x -> x
  end
[%%expect{|
class idfunc : object method id : ([< `A | `B ] as 'a) -> 'a end
|}]

let act : [ `A | `B ] -> [`A] =
 fun x -> (new idfunc)#id x
[%%expect{|
Line 2, characters 10-27:
2 |  fun x -> (new idfunc)#id x
              ^^^^^^^^^^^^^^^^^
Error: This expression has type [ `A | `B ]
       but an expression was expected of type [ `A ]
       The second variant type does not allow tag(s) `B
|}]
