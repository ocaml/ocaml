(* TEST
  * expect
*)

type ab = [ `A | `B ];;
let f (x : [`A]) = match x with #ab -> 1;;
[%%expect{|
type ab = [ `A | `B ]
Line _, characters 32-35:
Error: This pattern matches values of type [? `A | `B ]
       but a pattern was expected which matches values of type [ `A ]
       The second variant type does not allow tag(s) `B
|}];;
let f x = ignore (match x with #ab -> 1); ignore (x : [`A]);;
[%%expect{|
Line _, characters 31-34:
Error: This pattern matches values of type [? `B ]
       but a pattern was expected which matches values of type [ `A ]
       The second variant type does not allow tag(s) `B
|}, Principal{|
Line _, characters 31-34:
Error: This pattern matches values of type [? `B ]
       but a pattern was expected which matches values of type [ `A ]
       Types for tag `B are incompatible
|}];;
let f x = ignore (match x with `A|`B -> 1); ignore (x : [`A]);;
[%%expect{|
Line _, characters 34-36:
Error: This pattern matches values of type [? `B ]
       but a pattern was expected which matches values of type [ `A ]
       The second variant type does not allow tag(s) `B
|}, Principal{|
Line _, characters 34-36:
Error: This pattern matches values of type [? `B ]
       but a pattern was expected which matches values of type [ `A ]
       Types for tag `B are incompatible
|}];;

let f (x : [< `A | `B]) = match x with `A | `B | `C -> 0;; (* warn *)
[%%expect{|
Line _, characters 49-51:
Warning 12: this sub-pattern is unused.
val f : [< `A | `B ] -> int = <fun>
|}];;
let f (x : [`A | `B]) = match x with `A | `B | `C -> 0;; (* fail *)
[%%expect{|
Line _, characters 47-49:
Error: This pattern matches values of type [? `C ]
       but a pattern was expected which matches values of type [ `A | `B ]
       The second variant type does not allow tag(s) `C
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
