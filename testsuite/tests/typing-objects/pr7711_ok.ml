(* TEST
   * expect
*)

type 'a r = <w: int -> int; .. > as 'a;;
[%%expect{|
type 'a r = 'a constraint 'a = < w : int -> int; .. >
|}];;

class type virtual ct = object('self)
  constraint 'self = 'not_self r
end;;
[%%expect{|
class type virtual ct = object method virtual w : int -> int end
|}];;
