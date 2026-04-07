(* TEST
 expect;
*)

let x = ref []
module M = struct type t let _ = (x : t list ref) end;;
[%%expect{|
val x : '_weak1 list ref = {contents = []}
Line 2, characters 34-35:
2 | module M = struct type t let _ = (x : t list ref) end;;
                                      ^
Error: The value "x" has type "'weak1 list ref"
       but an expression was expected of type "t list ref"
       The type constructor "t" would escape its scope
|}]
