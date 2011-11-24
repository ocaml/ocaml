module type S = sig type t and s = t end;;
module type S' = S with type t := int;;

module type S = sig module rec M : sig end and N : sig end end;;
module type S' = S with module M := String;;

(* A subtle problem appearing with -principal *)
type -'a t
class type c = object method m : [ `A ] t end;;
module M : sig val v : (#c as 'a) -> 'a end =
  struct let v x = ignore (x :> c); x end;;
