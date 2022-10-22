(*****************)
(* Shared memory *)
(*****************)

module type T = sig
  type t
  val empty : 'a -> t
  val reinit :  t -> unit
end

module type S = sig
  type t
  val make : int -> t
  val reinit : t -> unit
  type in_t
  val env2in : t -> int -> in_t
end

(* One memory location *)
module One : functor (T0:T) -> S with type in_t = T0.t

(* Two memory locations *)
module Make :
functor (T0:T) ->
functor (T1:T) -> S with type in_t = T0.t * T1.t
