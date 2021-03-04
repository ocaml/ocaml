(* TEST
   * expect
*)

type 'p pair = 'a * 'b constraint 'p = < left:'a; right:'b>

(* New in 4.11 *)
let error: 'left 'right.
  <left:'left; right:'right> pair -> <left:'right; right:'left> pair =
  fun (x,y) -> (y,x)
[%%expect{|
type 'c pair = 'a * 'b constraint 'c = < left : 'a; right : 'b >
val error :
  < left : 'left; right : 'right > pair ->
  < left : 'right; right : 'left > pair = <fun>
|}]

(* Known problem with polymorphic methods *)
let foo :
  < m : 'left 'right. <left:'left; right:'right> pair >
   -> < m : 'left 'right. <left:'left; right:'right> pair >
= fun x -> x

[%%expect{|
Line 4, characters 11-12:
4 | = fun x -> x
               ^
Error: This expression has type
         < m : 'left 'right. < left : 'left; right : 'right > pair >
       but an expression was expected of type
         < m : 'left 'right. < left : 'left; right : 'right > pair >
       Type < left : 'left; right : 'right > pair = 'a * 'b
       is not compatible with type < left : 'left0; right : 'right0 > pair
       The method left has type 'a, but the expected method type was 'left
       The universal variable 'left would escape its scope
|}]
