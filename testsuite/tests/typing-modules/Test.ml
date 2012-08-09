module type S = sig type t and s = t end;;
module type S' = S with type t := int;;

module type S = sig module rec M : sig end and N : sig end end;;
module type S' = S with module M := String;;
