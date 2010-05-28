(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Luc Maranget, projet Moscova, INRIA Rocquencourt            *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Signature of iterators: modules that
    offer a functional iterator over collections (type [t])
    of elements (type [elt]) in a functional way *)

module type S = sig

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
end


(** An example: iterator over a list *)

module ListMake(E:sig type elt end) : sig
  type t = E.elt list
  type elt = E.elt
  type enum

  val start : t -> enum
  val step : enum -> (elt * enum) option  
end  

(** Appart from functor machinery, the code is rather trivial:

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
  
