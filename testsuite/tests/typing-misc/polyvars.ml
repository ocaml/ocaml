(* TEST
   * expect
*)

type ab = [ `A | `B ];;
let f (x : [`A]) = match x with #ab -> 1;;
[%%expect{|
type ab = [ `A | `B ]
Line _, characters 32-35:
  let f (x : [`A]) = match x with #ab -> 1;;
                                  ^^^
Error: This pattern matches values of type [? `A | `B ]
       but a pattern was expected which matches values of type [ `A ]
       The second variant type does not allow tag(s) `B
|}];;
let f x = ignore (match x with #ab -> 1); ignore (x : [`A]);;
[%%expect{|
Line _, characters 31-34:
  let f x = ignore (match x with #ab -> 1); ignore (x : [`A]);;
                                 ^^^
Error: This pattern matches values of type [? `B ]
       but a pattern was expected which matches values of type [ `A ]
       The second variant type does not allow tag(s) `B
|}];;
let f x = ignore (match x with `A|`B -> 1); ignore (x : [`A]);;
[%%expect{|
Line _, characters 34-36:
  let f x = ignore (match x with `A|`B -> 1); ignore (x : [`A]);;
                                    ^^
Error: This pattern matches values of type [? `B ]
       but a pattern was expected which matches values of type [ `A ]
       The second variant type does not allow tag(s) `B
|}];;

let f (x : [< `A | `B]) = match x with `A | `B | `C -> 0;; (* warn *)
[%%expect{|
Line _, characters 49-51:
  let f (x : [< `A | `B]) = match x with `A | `B | `C -> 0;; (* warn *)
                                                   ^^
Warning 12: this sub-pattern is unused.
val f : [< `A | `B ] -> int = <fun>
|}];;
let f (x : [`A | `B]) = match x with `A | `B | `C -> 0;; (* fail *)
[%%expect{|
Line _, characters 47-49:
  let f (x : [`A | `B]) = match x with `A | `B | `C -> 0;; (* fail *)
                                                 ^^
Error: This pattern matches values of type [? `C ]
       but a pattern was expected which matches values of type [ `A | `B ]
       The second variant type does not allow tag(s) `C
|}];;

(* imported from in poly.ml *)
type t = A | B;;
function `A,_ -> 1 | _,A -> 2 | _,B -> 3;;
function `A,_ -> 1 | _,(A|B) -> 2;;
function Some `A, _ -> 1 | Some _, A -> 2 | None, A -> 3 | _, B -> 4;;
function Some `A, A -> 1 | Some `A, B -> 1
       | Some _, A -> 2  | None, A -> 3 | _, B -> 4;;
function A, `A -> 1 | A, `B -> 2 | B, _ -> 3;;
function `A, A -> 1 | `B, A -> 2 | _, B -> 3;;
function (`A|`B), _ -> 0 | _,(`A|`B) -> 1;;
function `B,1 -> 1 | _,1 -> 2;;
function 1,`B -> 1 | 1,_ -> 2;;
[%%expect {|
type t = A | B
- : [> `A ] * t -> int = <fun>
- : [> `A ] * t -> int = <fun>
- : [> `A ] option * t -> int = <fun>
- : [> `A ] option * t -> int = <fun>
- : t * [< `A | `B ] -> int = <fun>
- : [< `A | `B ] * t -> int = <fun>
Line _, characters 0-41:
  function (`A|`B), _ -> 0 | _,(`A|`B) -> 1;;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(`AnyOtherTag, `AnyOtherTag)
- : [> `A | `B ] * [> `A | `B ] -> int = <fun>
Line _, characters 0-29:
  function `B,1 -> 1 | _,1 -> 2;;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(_, 0)
Line _, characters 21-24:
  function `B,1 -> 1 | _,1 -> 2;;
                       ^^^
Warning 11: this match case is unused.
- : [< `B ] * int -> int = <fun>
Line _, characters 0-29:
  function 1,`B -> 1 | 1,_ -> 2;;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(0, _)
Line _, characters 21-24:
  function 1,`B -> 1 | 1,_ -> 2;;
                       ^^^
Warning 11: this match case is unused.
- : int * [< `B ] -> int = <fun>
|}];;

(* PR#6787 *)
let revapply x f = f x;;

let f x (g : [< `Foo]) =
  let y = `Bar x, g in
  revapply y (fun ((`Bar i), _) -> i);;
(* f : 'a -> [< `Foo ] -> 'a *)
[%%expect{|
val revapply : 'a -> ('a -> 'b) -> 'b = <fun>
val f : 'a -> [< `Foo ] -> 'a = <fun>
|}];;

(* PR#6124 *)
let f : ([`A | `B ] as 'a) -> [> 'a] -> unit = fun x (y : [> 'a]) -> ();;
let f (x : [`A | `B] as 'a) (y : [> 'a]) = ();;
[%%expect{|
Line _, characters 61-63:
  let f : ([`A | `B ] as 'a) -> [> 'a] -> unit = fun x (y : [> 'a]) -> ();;
                                                               ^^
Error: The type 'a does not expand to a polymorphic variant type
Hint: Did you mean `a?
|}]

(* PR#5927 *)
type 'a foo = 'a constraint 'a = [< `Tag of & int];;
[%%expect{|
type 'a foo = 'a constraint 'a = [< `Tag of & int ]
|}]

(* PR#7704 *)
type t = private [> `A of string ];;
function (`A x : t) -> x;;
[%%expect{|
type t = private [> `A of string ]
Line _, characters 0-24:
  function (`A x : t) -> x;;
  ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`<some private tag>
- : t -> string = <fun>
|}]

let f = function `AnyOtherTag, _ -> 1 | _, (`AnyOtherTag|`AnyOtherTag') -> 2;;
[%%expect{|
Line _, characters 8-76:
  let f = function `AnyOtherTag, _ -> 1 | _, (`AnyOtherTag|`AnyOtherTag') -> 2;;
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(`AnyOtherTag', `AnyOtherTag'')
val f : [> `AnyOtherTag ] * [> `AnyOtherTag | `AnyOtherTag' ] -> int = <fun>
|}]
