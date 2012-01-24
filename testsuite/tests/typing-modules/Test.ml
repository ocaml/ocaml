(* Destructive substitutions *)

module type S = sig type t and s = t end;;
module type S' = S with type t := int;;

module type S = sig module rec M : sig end and N : sig end end;;
module type S' = S with module M := String;;

(* A subtle problem appearing with -principal *)
type -'a t
class type c = object method m : [ `A ] t end;;
module M : sig val v : (#c as 'a) -> 'a end =
  struct let v x = ignore (x :> c); x end;;

(* Path shortening *)

module Int = struct type t = int let compare : int -> int -> int = compare end;;

let f (x : Int.t) = x;;

f true;;

type 'a u constraint 'a = bool;;
let f (x : Int.t u) = ();;

let f (x : (Int.t as 'a) -> (bool as 'a)) = ();;

type t = [Int.t | `A];;

type t = [`A of Int.t | `A of bool];;
