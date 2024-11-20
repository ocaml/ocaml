(* TEST
 expect;
*)

(* #13579 *)

module F(X : sig type 'a t end) = struct
  type (_, _) gadt = T : ('a X.t, 'a) gadt

  let equate_param2_based_on_param1 (type tt m1 m2)
        (T : (tt, m1) gadt) (T : (tt, m2) gadt) : (m1, m2) Type.eq =
     Equal
  ;;
end
[%%expect{|
Line 6, characters 5-10:
6 |      Equal
         ^^^^^
Error: The constructor "Equal" has type "(m1, m1) Type.eq"
       but an expression was expected of type "(m1, m2) Type.eq"
       Type "m1" is not compatible with type "m2"
|}]

(* could cause unsoundness
module Z = F(struct type 'a t = unit end)

let () =
  let t1 = (Z.T : (unit, int) Z.gadt) in
  let t2 = (Z.T : (unit, string) Z.gadt) in
  let eq : (int, string) Type.eq = Z.equate_param2_based_on_param1 t1 t2 in
  let cast (type a b) (Equal : (a, b) Type.eq) (a : a) : b = a in
  print_string (cast eq 1)
;;
*)

(* Side-effect of the fix *)

module M = struct type 'a p end
type _ t = W: int M.p t
[%%expect{|
module M : sig type 'a p end
type _ t = W : int M.p t
|}]

let f (W: _ M.p t) = ()
[%%expect{|
Line 1, characters 7-8:
1 | let f (W: _ M.p t) = ()
           ^
Error: This pattern matches values of type "int M.p t"
       but a pattern was expected which matches values of type "$0 M.p t"
       The type constructor "$0" would escape its scope
|}]

let f (W: _ t) = ()
[%%expect{|
val f : int M.p t -> unit = <fun>
|}]
