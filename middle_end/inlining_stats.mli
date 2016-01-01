(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module Closure_stack : sig
  type t

  val create : unit -> t

  val note_entering_closure
     : t
    -> closure_id:Closure_id.t
    -> where:Inlining_stats_types.where_entering_closure
    -> t
end

val record_decision
   : Inlining_stats_types.Decision.t
  -> closure_stack:Closure_stack.t
  -> debuginfo:Debuginfo.t
  -> unit

val save_then_forget_decisions : output_prefix:string -> unit
