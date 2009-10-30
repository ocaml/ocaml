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
    - [m.register] is used by agents to indicate that they can perform
      computations, mapping [xi] values to [yi] results.
    - [m.wait] returns the combined result [comb y1 (comb y2 (... (comb yn y0)))],
      where the [yi] values being the results of the [xi] applied to the functions
      registered by the agents. The [xi] are the values successively returned by [e]. *)

val create : ('d, 'a) enum -> ('b -> 'c -> 'c) -> 'c -> ('a, 'b, 'c) t
(** [create e comb y0] returns a pool for computations of type ['a -> 'b],
    [comb] being used to combine results with initial result [y0].
    The enumeration [e] is used to generate the input values for the
    various computations. *)
