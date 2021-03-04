(* TEST
   * expect
*)

let f = function
  | ([] : int list) as x -> x
  | _ :: _ -> assert false;;
[%%expect{|
val f : int list -> int list = <fun>
|}]

let f =
  let f' = function
    | ([] : 'a list) as x -> x
    | _ :: _ -> assert false
  in
  f', f';;
[%%expect{|
val f : ('a list -> 'a list) * ('a list -> 'a list) = (<fun>, <fun>)
|}]

let f =
  let f' = function
    | ([] : _ list) as x -> x
    | _ :: _ -> assert false
  in
  f', f';;
[%%expect{|
val f : ('a list -> 'b list) * ('c list -> 'd list) = (<fun>, <fun>)
|}]

let f =
  let f' (type a) = function
    | ([] : a list) as x -> x
    | _ :: _ -> assert false
  in
  f', f';;
[%%expect{|
val f : ('a list -> 'a list) * ('b list -> 'b list) = (<fun>, <fun>)
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
    (* This should be flagged as non-exhaustive: because of the constraint [x]
       is of type [t]. *)
    begin match x with
    | `A -> ()
    end
  | `B -> ();;
[%%expect{|
Lines 5-7, characters 4-7:
5 | ....begin match x with
6 |     | `A -> ()
7 |     end
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
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

(* Make sure *all* the constraints are respected: *)

let f = function
  | ((`A : _) : t) as x ->
    (* This should be flagged as non-exhaustive: because of the constraint [x]
       is of type [t]. *)
    begin match x with
    | `A -> ()
    end
  | `B -> ();;
[%%expect{|
Lines 5-7, characters 4-7:
5 | ....begin match x with
6 |     | `A -> ()
7 |     end
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`B
val f : t -> unit = <fun>
|}]

let f = function
  | ((`A : t) : _) as x ->
    (* This should be flagged as non-exhaustive: because of the constraint [x]
       is of type [t]. *)
    begin match x with
    | `A -> ()
    end
  | `B -> ();;

[%%expect{|
Lines 5-7, characters 4-7:
5 | ....begin match x with
6 |     | `A -> ()
7 |     end
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`B
val f : t -> unit = <fun>
|}]
