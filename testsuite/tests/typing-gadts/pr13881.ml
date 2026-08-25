(* TEST
 expect;
*)

module Type1 = struct
  type t = [ `Foo of t list ]
end

module Type2 = struct
  type t = [ `Foo of t list ]
end

module type S = sig
  val conv : Type2.t -> int
end

module M = struct
  let conv : Type1.t -> int = fun _ -> assert false
end

type 'a rep = String : string rep

let return_m : type a. a rep -> (module S) =
  fun t ->
  match t with
  | String -> (module M)
;;

[%%expect{|
module Type1 : sig type t = [ `Foo of t list ] end
module Type2 : sig type t = [ `Foo of t list ] end
module type S = sig val conv : Type2.t -> int end
module M : sig val conv : Type1.t -> int end
type 'a rep = String : string rep
val return_m : 'a rep -> (module S) = <fun>
|}]
