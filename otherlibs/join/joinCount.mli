(***********************************************************************)
(*                                                                     *)
(*                           JoCaml                                    *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2008 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Count {i n} events programming idiom *)

(** Simple countdowns. *)
module Down :
  sig
    type ('a, 'b) t = { tick : unit Join.chan; wait : unit -> unit; }

    val create : int -> ('a, 'b) t
    (** [create n] returns a countdown [c] for [n] events.
        That is, after [n] messages on [c.tick], [c.wait()] will return *)
  end


(** Collecting countdowns, or {e collectors}. *)
module Collector :
  sig
    type ('a, 'b) t = { collect : 'a Join.chan; wait : unit -> 'b; }
    val create : ('a -> 'b -> 'b) -> 'b -> int -> ('a, 'b) t
    (** [create 
  end


(** Dynamic collectors *)
module Dynamic :
  sig
    type ('a, 'b) t = {
      enter : unit -> unit;
      leave : 'a Join.chan;
      wait : unit -> 'b;
      finished : unit Join.chan;
    }
    val create : ('a -> 'b -> 'b) -> 'b -> ('a, 'b) t
  end
