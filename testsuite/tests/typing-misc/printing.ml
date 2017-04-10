(* PR#7012 *)

type t = [ 'A_name | `Hi ];;
[%%expect{|
Line _, characters 11-18:
Error: The type 'A_name does not expand to a polymorphic variant type
Hint: Did you mean `A_name?
|}];;

let f (x:'id_arg) = x;;
[%%expect{|
val f : 'id_arg -> 'id_arg = <fun>
|}];;

let f (x:'Id_arg) = x;;
[%%expect{|
val f : 'Id_arg -> 'Id_arg = <fun>
|}];;
