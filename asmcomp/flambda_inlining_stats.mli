open Abstract_identifiers

module Closure_stack : sig
  type t

  val create : unit -> t

  val note_entering_closure
     : t
    -> closure_id:Closure_id.t
    -> where:Flambda_inlining_stats_types.where_entering_closure
    -> t
end

val record_decision
   : Flambda_inlining_stats_types.Decision.t
  -> closure_stack:Closure_stack.t
  -> debuginfo:Debuginfo.t
  -> unit

val save_then_forget_decisions : output_prefix:string -> unit
