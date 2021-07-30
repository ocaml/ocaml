(* TEST
   * expect
*)

open Either;;

[left 1; right true];;
[%%expect {|
- : (int, bool) Either.t list = [Left 1; Right true]
|}];;

List.map is_left [left 1; right true];;
[%%expect {|
- : bool list = [true; false]
|}];;

List.map is_right [left 1; right true];;
[%%expect {|
- : bool list = [false; true]
|}];;

[find_left (Left 1); find_left (Right 1)];;
[%%expect {|
- : int option list = [Some 1; None]
|}];;

[find_right (Left 1); find_right (Right 1)];;
[%%expect {|
- : int option list = [None; Some 1]
|}];;

[map_left succ (Left 1); map_left succ (Right true)];;
[%%expect {|
- : (int, bool) Either.t list = [Left 2; Right true]
|}];;

[map_right succ (Left ()); map_right succ (Right 2)];;
[%%expect {|
- : (unit, int) Either.t list = [Left (); Right 3]
|}];;

[map ~left:succ ~right:not (Left 1);
 map ~left:succ ~right:not (Right true)];;
[%%expect {|
- : (int, bool) Either.t list = [Left 2; Right false]
|}];;

[fold ~left:succ ~right:int_of_string (Left 1);
 fold ~left:succ ~right:int_of_string (Right "2")];;
[%%expect {|
- : int list = [2; 2]
|}];;

let li = ref [] in
let add to_str x = li := to_str x :: !li in
iter ~left:(add Fun.id) ~right:(add string_of_int) (Left "foo");
iter ~left:(add Fun.id) ~right:(add string_of_int) (Right 2);
List.rev !li;;
[%%expect {|
- : string list = ["foo"; "2"]
|}];;

(
  for_all ~left:((=) 1) ~right:((=) "foo") (Left 1),
  for_all ~left:((=) 1) ~right:((=) "foo") (Right "foo"),
  for_all ~left:((=) 1) ~right:((=) "foo") (Left 2),
  for_all ~left:((=) 1) ~right:((=) "foo") (Right "bar")
);;
[%%expect {|
- : bool * bool * bool * bool = (true, true, false, false)
|}];;

equal ~left:(=) ~right:(=) (Left 1) (Left 1),
equal ~left:(=) ~right:(=) (Right true) (Right true);;
[%%expect {|
- : bool * bool = (true, true)
|}];;

(equal ~left:(=) ~right:(=) (Left 1) (Left 2),
 equal ~left:(=) ~right:(=) (Right true) (Right false),
 equal ~left:(=) ~right:(=) (Left 1) (Right true),
 equal ~left:(=) ~right:(=) (Right 1) (Left true));;
[%%expect {|
- : bool * bool * bool * bool = (false, false, false, false)
|}];;

equal ~left:(fun _ _ -> false) ~right:(=) (Left 1) (Left 1),
equal ~left:(=) ~right:(fun _ _ -> false) (Right true) (Right true);;
[%%expect {|
- : bool * bool = (false, false)
|}];;

let cmp = Stdlib.compare in
(
 (compare ~left:cmp ~right:cmp (Left 0) (Left 1),
  compare ~left:cmp ~right:cmp (Left 1) (Left 1),
  compare ~left:cmp ~right:cmp (Left 1) (Left 0)),

 (compare ~left:cmp ~right:cmp (Right 0) (Right 1),
  compare ~left:cmp ~right:cmp (Right 1) (Right 1),
  compare ~left:cmp ~right:cmp (Right 1) (Right 0)),

 (compare ~left:cmp ~right:cmp (Left 1) (Right true),
  compare ~left:cmp ~right:cmp (Right 1) (Left true))
);;
[%%expect {|
- : (int * int * int) * (int * int * int) * (int * int) =
((-1, 0, 1), (-1, 0, 1), (-1, 1))
|}];;
