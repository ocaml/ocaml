(* TEST
 expect;
*)

type 'a t = T of 'a
type 'a s = S of 'a

type (_, _) eq = Refl : ('a, 'a) eq;;

let f : (int s, int t) eq -> unit = function Refl -> ();;
[%%expect{|
type 'a t = T of 'a
type 'a s = S of 'a
type (_, _) eq = Refl : ('a, 'a) eq
Line 6, characters 45-49:
6 | let f : (int s, int t) eq -> unit = function Refl -> ();;
                                                 ^^^^
Error: This pattern matches values of type "(int s, int s) eq"
       but a pattern was expected which matches values of type
         "(int s, int t) eq"
       Type "int s" is not compatible with type "int t"
|}];;

module M (S : sig type 'a t = T of 'a type 'a s = T of 'a end) =
struct let f : ('a S.s, 'a S.t) eq -> unit = function Refl -> () end;;
[%%expect{|
Line 2, characters 54-58:
2 | struct let f : ('a S.s, 'a S.t) eq -> unit = function Refl -> () end;;
                                                          ^^^^
Error: This pattern matches values of type "($'a S.s, $'a S.s) eq"
       but a pattern was expected which matches values of type
         "($'a S.s, $'a S.t) eq"
       The type constructor "$'a" would escape its scope
|}];;
