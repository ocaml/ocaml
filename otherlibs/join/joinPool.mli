(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Dynamic dispatch of work among registered agents. *)


(** Simple implementation: use-once pools that can be used
    to dispatch one task. *)
module Simple : sig
(** {6 Enumerations} *)

type ('a, 'b) enum =
  { start : unit -> 'a;
    step : 'a -> ('b * 'a) option; }
(** Enumerations iterate over values in a functional way;
    ['a] is the type of the enumeration state,
    while ['b] is the type of the enumeration elements.

    [start] should return a new state pointing to the start of the enumeration.

    [step] should map a state to a (current element, next state) couple,
    returning [None] if there is no more element to return. *)

val enum_of_interval : int -> int -> (int, int) enum
(** [enum_of_interval inf sup] returns an enumeration that will iterate
    the interval from [int] to [sup]. *)

val enum_of_list : 'a list -> ('a list, 'a) enum
(** [enum_of_list l] returns an enumeration that will iterate over the
    elements of [l]. *)


(** {6 Pools} *)

type ('a, 'b, 'c) t =
  { register : ('a -> 'b) Join.chan;
    wait : unit -> 'c; }
(** Pools dispatch computations among registered agents, re-issuing pending
    tasks if agents do not send computation outcomes.

    Given a pool [p], returned by [create e comb y0]:
    - [p.register f] is used by agents to indicate that they can perform
      computations [f], mapping [xi] values to [yi] results.
    - [p.wait ()] returns the combined result [comb y1 (comb y2 (... (comb yn y0)))],
      where the [yi] values are
      the results of the [xi] applied to the functions
      registered by the agents. The [xi] are the values returned by
      the enumumeration specified at pool creation time. *)

val create : ('d, 'a) enum -> ('b -> 'c -> 'c) -> 'c -> ('a, 'b, 'c) t
(** [create e comb y0] returns a pool for computations of type ['a -> 'b],
    [comb] being used to combine results with initial result [y0].
    The enumeration [e] is used to generate the input values for the
    various computations. *)

end


(** Advanced implementation with functorial interface *)
module Shared : sig

(** This module provides advanced task management.
    Pools dispatch computations among registered agents, re-issuing pending
    tasks if agents do not send computation outcomes.
    It improves over the more simple {!Simple} in the following aspects:
    - The same pool can be shared by several computations.
    - More efficient handling of task re-issuing: fresh tasks have priority
      over re-issued tasks.
    - Ability to abort duplicated tasks when outcome reaches the pool.
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
(** A given task will be re-issued at most [nagain] times.
    No limit is enforced when [nagain] is strictly less that zero *)
end

(** Functional enumerations *)
module type Enumerable = sig


(** Signature of iterators: modules that
    offer a functional iterator over collections (type [t])
    of elements (type [elt]) in a functional way *)

  type t     (** Collection *)
  type elt   (** Elements in the collection *)
  type enum  (** Explicit state. *)


  val start : t -> enum
  (** Start iterating over a collection, [start c]
      returns the initial state *)

  val step : enum -> (elt * enum) option
  (** Iterate once, [step st] returns [None] when iteration is over,
      or [Some (e,st')] otherwise, where [e] is the next element and
      [st']  is the next explicit state. *)

(** An example: iterator over integer intervals:

{[module Interval = struct
  type t = { low : int; high : int; } (* Interval (low..high) *)
  type elt = int
  type enum = { next : int; max : int; }

  let start x = { next=x.low; max=x.high; }

  let step x =
    if x.next > x.max then None
    else Some (x.next, { x with next=x.next+1; })
end]}
*)

(** Another example: iterator over a list:

{[module  ListMake(E:sig type elt end) = struct
  type t = E.elt list
  type elt = E.elt
  type enum = t

  let start xs = xs

  let step = function
  | [] -> None
  | x::xs -> Some (x,xs)

end]}

*)

end


  

(** {6 Pools} *)

  type ('elt,'partial) worker = 'elt -> 'partial
(** Standard workers *)

  type subtask_id = int (** Subtask identifier *)

  type ('elt,'partial) interruptible_worker =
      subtask_id * 'elt -> 'partial option (** Workers that can be aborted asynchronously *)
  type kill = subtask_id Join.chan (** To abort given subtask *)


(** Output signature of the pool functor *)
module type S = sig
  type elt         (** Element from a collection *)
  type collection  (** Collection *)

  type ('partial, 'result) t = {
      register : (elt,'partial) worker Join.chan;
      register_interruptible :
        ((elt,'partial) interruptible_worker * kill) Join.chan;
      fold :
        collection -> ('partial -> 'result -> 'result) -> 'result -> 'result;
    }
(** Pools dispatch computations among registered agents, re-issuing pending
    tasks if agents do not send computation outcomes.

    Given a pool [p], returned by [create ()]:
    - [p.register w] is used by agents to indicate that they can perform
      computations, mapping [xi] values to [yi] results, using the
      synchronous channel [w].
    - [p.fold  c comb y0] returns the combined result
      [comb y1 (comb y2 (... (comb yn y0)))],
      where the [yi] values are the results of the [xi]
      transformed by the functions
      registered by the agents. The [xi] result from enumerating the collection
      [c]. The enumeration technique is specified by the module argument [E]
      (signature {!Enumerable})
      to the functor {!Make}.
    - [p.register_interruptible (w,k)] is used by agents to indicate that
      they can perform computations as above.
      Additionally the pool logics will attempt to abort computations
      found to be useless by issusing messages on channel [k].
      More specifically, when
      given an argument [(id,xi)] by the pool logics,
      the synchronous channel [w] should return [Some yi],
      where [xi] and [yi] are the same as in the description of [p.fold] above.
      However, if the pool sends [id] on channel [k] before [yi] is
      available, then the agent may abort the computation of [yi],
      so as to spare computing power.
      In that case, [w(xi)] must reply [None].
      It is the agent responsability to check that subtask identifiers
      sent on the [w] and [k] channels
      are equal before aborting the subtask and having [w] to reply [None] *)
     

  val create : unit ->  ('partial, 'result) t 
  (** Pool creator *)
end

(** Functor to build pools, given an enumeration technique specification ([E]) *)module Make (C:Config) (E:Enumerable) :
    S with type elt = E.elt and type collection = E.t

end
