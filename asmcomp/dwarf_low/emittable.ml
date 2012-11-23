open Std_internal

module type S = sig
  type t

  val size : t -> int
  val emit : t -> emitter:Emitter.t -> unit
end
