(* TEST
 expect;
*)

[@@@warning "-16"]

let f ?x:_ _ = assert false;;

(* This should be fine. There's a positional argument next, so we know that
erasing the optional argument is okay. *)
let _ = (f : 'a -> 'a);;

[%%expect{|
val f : ?x:'a -> 'b -> 'c = <fun>
- : 'a -> 'a = <fun>
|}];;

let f ?x:_ ~y:_ = 1;;

(* This shouldn't be allowed - we're erasing an unerasable optional argument. *)
let _ = (f : y:'a -> int);;

[%%expect {|
val f : ?x:'a -> y:'b -> int = <fun>
Line 4, characters 9-10:
4 | let _ = (f : y:'a -> int);;
             ^
Error: The value "f" has type "?x:'b -> y:'c -> int"
       but an expression was expected of type "y:'a -> int"
       Labels "?x" and "y" do not match
|}];;

(* This shouldn't be allowed, either. The optional argument is erasable, but it
could get erased without applying the positional argument if only the labeled
argument is passed in after the typecast. *)
let f ?x:_ ~y:_ _ = 1;;

let _ = (f : y:_ -> _ -> int);;

[%%expect {|
val f : ?x:'a -> y:'b -> 'c -> int = <fun>
Line 3, characters 9-10:
3 | let _ = (f : y:_ -> _ -> int);;
             ^
Error: The value "f" has type "?x:'a -> y:'b -> 'c -> int"
       but an expression was expected of type "y:'d -> 'e -> int"
       Labels "?x" and "y" do not match
|}];;
