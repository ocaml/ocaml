(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(*
  Abstract type of join-definitions.
  Basically, a join-definition contains
    * Some messages queues, one per defined channel, sorted by
      increasing channel indexes (0, 1, ... )
    * Some ``matches'', one per defined channel, sorted by
      increasing channel indexes (0, 1, ... )
    * Some ``guards'' one per guarded process, sorted by
      increasing guarded process indexes (0,1, ...)

  The numbers of channels and guarded processes are given while creating
  the join-definitions.
*)
type automaton

(*
  Match tables, they are vectors, containing integers,
    m1, i1, m2, i2, etc.
    where - mi is a matching pattern, encoded as a bit field of
            channel indexes.
          - ii is the index of te match guard
*)
type matches

(* Guards are functions.
Given a clause
   chan_1 arg_1 & chan_2 arg_2 & ... & chan_n arg_n = P
(elementary join patterns are sorted by increasing channel indexes)
the guard is somthing as:
   (fun pat_1 pat_2 ... pat_n -> compilation of P)
where pat_i is
   - arg_i when chan_i is asynchronous.
   - (cont_i, arg_i) when chan_i is synchronous.
*)
type guard

(* Type of continuations for synchronous channels *)
type continuation

(* Creating threads *)
external send_sync : automaton -> int -> 'a -> unit = "send_sync"
external send_async : automaton -> int -> 'a -> unit = "send_async"

(* create_automatons nchannels nguards *)
external create_automaton : int -> int -> automaton = "create_automaton"
external create_automaton_location : location -> int -> int -> automaton = "create_automaton_location"
external patch_match : automaton -> int -> matches -> unit = "patch_match"
external patch_guard : automaton -> int -> guard -> unit = "patch_guard"
external reply_to : 'a -> continuation -> unit = "reply_to"

(* IMPLEMENTED *)
  
val halt : unit -> unit
val create_process : (unit -> unit) -> unit
val create_process_location : location -> (unit -> unit) -> unit
val create_location : unit -> location

(* SHOULD NOT BE USED !!!
val exit : unit -> unit
*)