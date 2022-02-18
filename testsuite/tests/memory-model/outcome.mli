(*********************************************)
(* Outcomes, _i.e._ final values of test run *)
(*********************************************)

module type T = sig
  type t
  val compare : t -> t -> int
  val pp : t -> string
end

module type Allow = sig val allowed : bool end

module Int : T with type t = int

module Make :
functor(T0:T) ->
functor(T1:T) ->
functor
  (N:
     sig
       val name : string
       val tag0 : string
       val tag1 : string
       val ok : T0.t -> T1.t -> bool
     end) ->
functor
  (A:Allow) ->
    sig

    type t

    val compare : t -> t -> int

    val ok : t -> bool

    val allowed : bool

    val name : string

    val make : T0.t -> T1.t -> t

    val pp : out_channel -> t -> unit
  end

module OK : Allow
module NO : Allow
