(* TEST
   * expect
*)

let f = function ([] : int list) as x -> x ;;
[%%expect{|
Line 1, characters 8-42:
1 | let f = function ([] : int list) as x -> x ;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
_::_
val f : int list -> int list = <fun>
|}]

let f =
  let f' = function ([] : 'a list) as x -> x in
  f', f';;
[%%expect{|
Line 2, characters 11-44:
2 |   let f' = function ([] : 'a list) as x -> x in
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
_::_
val f : ('a list -> 'a list) * ('a list -> 'a list) = (<fun>, <fun>)
|}]

let f =
  let f' = function ([] : _ list) as x -> x in
  f', f';;
[%%expect{|
Line 2, characters 11-43:
2 |   let f' = function ([] : _ list) as x -> x in
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
_::_
val f : ('a list -> 'b list) * ('c list -> 'd list) = (<fun>, <fun>)
|}]

type t = [ `A | `B ];;
[%%expect{|
type t = [ `A | `B ]
|}]

let f = function `A as x -> x | `B -> `A;;
[%%expect{|
val f : [< `A | `B ] -> [> `A ] = <fun>
|}]

let f = function (`A : t) as x -> x | `B -> `A;;
[%%expect{|
val f : t -> t = <fun>
|}]

let f : t -> _ = function `A as x -> x | `B -> `A;;
[%%expect{|
val f : t -> [> `A ] = <fun>
|}]

let f = function
  | (`A : t) as x ->
    begin match x with
    | `A -> ()
    end
  | `B -> ();;
[%%expect{|
Lines 3-5, characters 4-7:
3 | ....begin match x with
4 |     | `A -> ()
5 |     end
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`B
val f : t -> unit = <fun>
|}]


let f = function
  | (`A : t) as x ->
    begin match x with
    | `A -> ()
    | `B -> ()
    end
  | `B -> ();;
[%%expect{|
val f : t -> unit = <fun>
|}]


let f = function
  | (`A : t) as x ->
    begin match x with
    | `A -> ()
    | `B -> ()
    | `C -> ()
    end
  | `B -> ();;
[%%expect{|
Line 6, characters 6-8:
6 |     | `C -> ()
          ^^
Error: This pattern matches values of type [? `C ]
       but a pattern was expected which matches values of type t
       The second variant type does not allow tag(s) `C
|}]

let f = function (`A, _ : _ * int) as x -> x;;
[%%expect{|
val f : [< `A ] * int -> [> `A ] * int = <fun>
|}]
