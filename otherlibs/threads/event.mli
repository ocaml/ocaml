(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*  David Nowak and Xavier Leroy, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Event]: first-class synchronous communication *)

(* This module implements synchronous interprocess communications over
   channels. As in John Reppy's Concurrent ML system, the communication 
   events are first-class values: they can be built and combined
   independently before being offered for communication. *)

type 'a channel
        (* The type of communication channels carrying values of type ['a]. *)
val new_channel: unit -> 'a channel
        (* Return a new channel. *)

type 'a event
        (* The type of communication events returning a result of type ['a]. *)
val send: 'a channel -> 'a -> unit event
        (* [send ch v] returns the event consisting in sending the value [v]
           over the channel [ch]. The result value of this event is [()]. *) 
val receive: 'a channel -> 'a event
        (* [receive ch] returns the event consisting in receiving a value
           from the channel [ch]. The result value of this event is the
           value received. *) 
val choose: 'a event list -> 'a event
        (* [choose evl] returns the event that is the alternative of
           all the events in the list [evl]. *)
val wrap: 'a event -> ('a -> 'b) -> 'b event
        (* [wrap ev fn] returns the event that performs the same communications
           as [ev], then applies the post-processing function [fn]
           on the return value. *)
val guard: (unit -> 'a event) -> 'a event
        (* [guard fn] returns the event that, when synchronized, computes
           [fn()] and behaves as the resulting event. This allows to
           compute events with side-effects at the time of the synchronization
           operation. *)
val sync: 'a event -> 'a
        (* ``Synchronize'' on an event: offer all the communication 
           possibilities specified in the event to the outside world,
           and block until one of the communications succeed. The result
           value of that communication is returned. *)
val select: 'a event list -> 'a
        (* ``Synchronize'' on an alternative of events.
           [select evl] is shorthand for [sync(choose el)]. *)
val poll: 'a event -> 'a option
        (* Non-blocking version of [sync]: offer all the communication 
           possibilities specified in the event to the outside world,
           and if one can take place immediately, perform it and return
           [Some r] where [r] is the result value of that communication.
           Otherwise, return [None] without blocking. *)
