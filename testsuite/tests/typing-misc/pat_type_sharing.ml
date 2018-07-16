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
Line _, characters 25-26:
    | Int, { a = 3; b } -> b
                           ^
Error: This expression has type a = int
       but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]
