(* $Id$ *)

class timed :  ?nocase:bool -> ?wait:int -> string list -> object
  val mutable texts : string list
  method add : string -> int
  method current : int
  method get_current : string
  method reset : unit
end
