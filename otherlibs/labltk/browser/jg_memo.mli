(* $Id$ *)

class ['a, 'b] c : fun:('a -> 'b) -> object
  val hash : ('a, 'b) Hashtbl.t
  method clear : unit
  method get : 'a -> 'b
  method reget : 'a -> 'b
end
