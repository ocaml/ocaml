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

(** Concurrent fifo buffers.

  Concurrent fifo's offer blocking [get] operations.
  More precisely, [get] operations on, active, empty fifo's are
  blocking.

  Fifo behavior can be observed in the simple situation where
  one agent ({e producer}) is putting elements,
  while another agent ({e consumer}) is getting them.
  Then the following guarantees hold:
    - The consumer sees the elements in producing order.
    - If the producer closes the fifo, and does not put any additional
      elements, then the consumer will
      have retrieved all elements when the call to [close] returns.

*)


type 'a t = {
  put : ('a * bool Join.chan) Join.chan;
(** Put element into fifo. *)    
  get : 'a option Join.chan Join.chan;
(** Get element from fifo. *)    
  close : unit -> unit;
(** Close fifo, returns only when the fifo is empty *)
  kill : unit Join.chan;
(** Close fifo, returns immediately, discarding any pending element. *)
}

(** The type of fifo buffer.  *)

val create : unit -> 'a t
(** Create a new fifo buffer. *)

(**
  Interface to concurrent fifo's is mostly asynchronous.
  Let [f] be a fifo.
    - [f.put(x,k)] put [v] into the fifo [f]. The channel
      [k] receives a (boolean) message [b], where:
         {ul {- If [b] is [true], then [v] was succesfully entered into [f].}
           {- If [b] is [false], then [v] could not be added to [f].
           That is, [f] have been closed or killed.}}
    - [f.get(k)] retrieve one element from the fifo, The channel [k]
      receives a (['a option)] message, where:
          {ul {- [None] expresses that the fifo is closed.}
        {- [Some x] expresses that element [x] is retrieved from the fifo.}}

  Operations [close] and [kill] both close the fifo, but with different
  behaviors as regards non-empty fifos.
   - [f.close()] waits for the fifo to be empty before closing it and
     returning.
   - [f.kill()] is an asynchronous channel, sending a message on
     [f.kill] closes the fifo immediately.

  In the producer/consumer scheme, [f.close] is for the producer to
  signal the end of produced elements; while [f.kill] is for the consummer
  to signal that it will not accept more elements.
*)     




(** {6 Producer/consumer interface} *)

val create_prod_cons : unit ->  'a JoinCom.P.t * 'a JoinCom.C.t
(** Create a pair of producer/consumer connected by
    a fifo. [create_prod_cons ()] returns the pair [prod,cons].

    The producer [prod] has the get and kill operations,
    while the consumer has the put and close operations.
    Indeed, by the convention of module {!JoinCom},
    [prod] produces data from the fifo, while [cons] consumes
    data to feed the fifo. *)

(** {6 Synchronous interface} *)

(** Type of fifo with synchronous operations *)

module S : sig
  exception Closed

  type 'a t =
    { put : 'a -> unit ;
      get :  unit -> 'a ;
      close : unit -> unit ;
      kill : unit -> unit ; }
end


val create_sync : unit -> 'a S.t
(** Records of type 'a S.t offer the same fields as the ones of
    type ['a t], but they hold synchronous channels (of functional
    type)

     - [put] of type ['a -> unit] either returns (when successful)
       or raise the exception [S.Closed].
     - [get] follows the same behavior.
     - [close] of type [unit -> unit] closes the fifo,
       returning when the fifo is empty.
*)       
       

