(* TEST
 expect;
*)

type t = < x : 'a. int as 'a >
[%%expect {|
Line 1, characters 15-28:
1 | type t = < x : 'a. int as 'a >
                   ^^^^^^^^^^^^^
Error: The universal type variable "'a" cannot be generalized: it is bound to
       "int".
|}]
type u = < x : 'a 'b. 'a as 'b >
[%%expect {|
Line 1, characters 15-30:
1 | type u = < x : 'a 'b. 'a as 'b >
                   ^^^^^^^^^^^^^^^
Error: The universal type variable "'b" cannot be generalized:
       it is already bound to another variable.
|}]
type v = 'b -> < x : 'a. 'b as 'a >
[%%expect {|
Line 1, characters 21-33:
1 | type v = 'b -> < x : 'a. 'b as 'a >
                         ^^^^^^^^^^^^
Error: The universal type variable "'a" cannot be generalized:
       it escapes its scope.
|}]


(** Check that renaming universal type variable is properly tracked
    in printtyp *)

let f (x:<a:'a; b:'a. 'a>) (y:<a:'a;b:'a>) = x = y
[%%expect {|
Line 4, characters 49-50:
4 | let f (x:<a:'a; b:'a. 'a>) (y:<a:'a;b:'a>) = x = y
                                                     ^
Error: The value "y" has type "< a : 'a; b : 'a >"
       but an expression was expected of type "< a : 'a; b : 'a0. 'a0 >"
       The method "b" has type "'a", but the expected method type was "'a0. 'a0"
       The universal variable "'a0" would escape its scope
|}]


(** MPR 7565 *)
class type t_a = object
    method f: 'a. 'a -> int
  end
let f (o:t_a) = o # f 0
let _ = f (object
    method f _ = 0
 end);;
[%%expect {|
class type t_a = object method f : 'a -> int end
val f : t_a -> int = <fun>
Lines 5-7, characters 10-5:
5 | ..........(object
6 |     method f _ = 0
7 |  end)..
Error: This expression has type "< f : 'a -> int >"
       but an expression was expected of type "t_a"
       The method "f" has type "'a -> int", but the expected method type was
       "'a0. 'a0 -> int"
       The universal variable "'a0" would escape its scope
|}
]

type uv = [ `A of <f: 'a. 'a -> int > ]
type 'a v = [ `A of <f: 'a -> int > ]
let f (`A o:uv) = o # f 0
let () = f ( `A (object method f _ = 0 end): _ v);;
[%%expect {|
type uv = [ `A of < f : 'a. 'a -> int > ]
type 'a v = [ `A of < f : 'a -> int > ]
val f : uv -> int = <fun>
Line 4, characters 11-49:
4 | let () = f ( `A (object method f _ = 0 end): _ v);;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a v" but an expression was expected of type
         "uv"
       The method "f" has type "'a -> int", but the expected method type was
       "'a0. 'a0 -> int"
       The universal variable "'a0" would escape its scope
|}]

(* Issue #8702: row types unified with universally quantified types*)

let f: 'a. ([> `A ] as 'a) -> [ `A ] = fun x -> x
[%%expect {|
Line 1, characters 48-49:
1 | let f: 'a. ([> `A ] as 'a) -> [ `A ] = fun x -> x
                                                    ^
Error: The value "x" has type "[> `A ]" but an expression was expected of type
         "[ `A ]"
       The first variant type is bound to the universal type variable "'a",
       it cannot be closed
|}]

let f: 'a. [ `A ] -> ([> `A ] as 'a) = fun x -> x
[%%expect {|
Line 1, characters 48-49:
1 | let f: 'a. [ `A ] -> ([> `A ] as 'a) = fun x -> x
                                                    ^
Error: The value "x" has type "[ `A ]" but an expression was expected of type
         "[> `A ]"
       The second variant type is bound to the universal type variable "'a",
       it cannot be closed
|}]


let f: 'a. [ `A | `B ] -> ([> `A ] as 'a) = fun x -> x
[%%expect {|
Line 1, characters 53-54:
1 | let f: 'a. [ `A | `B ] -> ([> `A ] as 'a) = fun x -> x
                                                         ^
Error: The value "x" has type "[ `A | `B ]"
       but an expression was expected of type "[> `A ]"
       The second variant type is bound to the universal type variable "'a",
       it cannot be closed
|}]


let f: 'a. [> `A | `B | `C ] -> ([> `A ] as 'a) = fun x -> x
[%%expect {|
Line 1, characters 59-60:
1 | let f: 'a. [> `A | `B | `C ] -> ([> `A ] as 'a) = fun x -> x
                                                               ^
Error: The value "x" has type "[> `A | `B | `C ]"
       but an expression was expected of type "[> `A ]"
       The second variant type is bound to the universal type variable "'a",
       it may not allow the tag(s) "`B", "`C"
|}]
