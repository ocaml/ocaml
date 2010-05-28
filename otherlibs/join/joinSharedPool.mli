(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Clerc, Luc Maranget, projet Moscova, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Advanced dynamic dispatch of work among registered agents. *)

(** This modules provides advanced task management.
    Pools dispatch computations among registered agents, re-issuing pending
    tasks if agents do not send computation outcomes.
    It improves over the more simple {!JoinPool} in the following aspects:
    - The same pool can be shared by several computations.
    - More efficient handling of task re-issuing: fresh tasks have priority
      over re-issued tasks.
    - A little control on pool behavior is offered by the means
      of the {!Config} module
      argument.
*)

(** {6 Arguments} *)


(** Configuration of pool *)
module type Config = sig
  val debug : bool
(** If true, gives a few diagnostics on the standard error stream. *)
  val nagain : int
(** A given task will be re-issued at most [nagain] times *)
end


(** Functional enumerations *)

(** {6 Pools} *)

module Make (C:Config) (E:Iterator.S) : sig

  type ('partial, 'result) t = {
      register : (E.elt -> 'partial) Join.chan;
      fold : E.t -> ('partial -> 'result -> 'result) -> 'result -> 'result;
    }

  val create : unit -> ('partial, 'result) t
end
