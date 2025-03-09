(****************)
(* Run one test *)
(****************)

(* Litmus test description, two threads *)

module type Test = sig

  module Key : Hist.Key (* Outcome *)

  module Env : Shared.S (* Shared memory *)

  (* First thread code *)
  type out0
  val code0 : Env.in_t -> out0

  (* Second thread code *)
  type out1
  val code1 : Env.in_t -> out1

  (* Build outcome from memory and thread results *)
  val out2key : Env.in_t -> out0 -> out1 -> Key.t
end

module Make :
functor (C:Opt.Config) ->
functor (T:Test) ->
sig
  val zyva : unit -> unit
end
