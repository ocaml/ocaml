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
