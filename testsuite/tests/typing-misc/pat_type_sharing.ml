(* TEST
   * expect
*)
type 'a r = { a : 'a; b : 'a; }
type 'a ty = Int : int ty | Float : float ty;;
[%%expect{|
type 'a r = { a : 'a; b : 'a; }
type 'a ty = Int : int ty | Float : float ty
|}]

let foo (type a) (ty : a ty) (x : a r) =
  match ty, x with
  | Int, { a = 3; b } -> b
  | _ -> assert false;;
[%%expect{|
val foo : 'a ty -> 'a r -> 'a = <fun>
|}]
