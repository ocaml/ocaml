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
val f : int list -> 'a list = <fun>
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
val f : ('a list -> 'b list) * ('a list -> 'c list) = (<fun>, <fun>)
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
val f : t -> [> `A ] = <fun>
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
val f : t -> unit = <fun>
|}]
