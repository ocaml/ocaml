(*****************************************************)
(* Histogram for collecting counts of final outcomes *)
(*****************************************************)

module type Key = sig
  type t
  val compare : t -> t -> int
  val ok : t -> bool
  val allowed : bool
  val name : string
  val pp : out_channel -> t -> unit
end

module Make:
functor
  (Cfg:sig val verbose : bool end) ->
  functor (K:Key) ->
     sig
       type t
       type key = K.t
       val empty : t
       val see : key -> t -> t
       val pp : out_channel -> t -> unit
       val union : t -> t -> t
     end
