(* TEST
 expect;
*)

type empty = Empty and filled = Filled
type ('a,'fout,'fin) opt =
  | N : ('a, 'f, 'f) opt
  | Y : 'a -> ('a, filled, empty) opt
type 'fill either =
  | Either : (string, 'fill, 'f) opt * (int, 'f, empty) opt -> 'fill either;;
[%%expect{|
type empty = Empty
and filled = Filled
type ('a, 'fout, 'fin) opt =
    N : ('a, 'f, 'f) opt
  | Y : 'a -> ('a, filled, empty) opt
type 'fill either =
    Either : (string, 'fill, 'f) opt * (int, 'f, empty) opt -> 'fill either
|}]

let f (* : filled either -> string *) =
  fun (Either (Y a, N)) -> a;;
[%%expect{|
Line 2, characters 6-23:
2 |   fun (Either (Y a, N)) -> a;;
          ^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Either (N, Y _)"

val f : filled either -> string = <fun>
|}]
