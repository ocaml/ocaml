open Abstract_identifiers

include module type of Flambda_inlining_stats_types

module Closure_stack : sig
  type t

  val create : unit -> t

  val note_entering_closure
     : t
    -> closure_id:Closure_id.t
    -> where:where_entering_closure
    -> t
end

val record_decision
   : Decision.t
  -> closure_stack:Closure_stack.t
  -> debuginfo:Debuginfo.t
  -> unit

val save_then_forget_decisions : output_prefix:string -> unit
