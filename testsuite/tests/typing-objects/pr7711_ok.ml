(* TEST
   * toplevel
*)

type 'a r = <w: int -> int; .. > as 'a;;

class type virtual ct = object('self)
  constraint 'self = 'not_self r
end;;
