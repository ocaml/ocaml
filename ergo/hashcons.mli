(**************************************************************************)
(*                                                                        *)
(*     The Alt-ergo theorem prover                                        *)
(*     Copyright (C) 2006-2010                                            *)
(*                                                                        *)
(*     Sylvain Conchon                                                    *)
(*     Evelyne Contejean                                                  *)
(*     Stephane Lescuyer                                                  *)
(*     Mohamed Iguernelala                                                *)
(*     Alain Mebsout                                                      *)
(*                                                                        *)
(*     CNRS - INRIA - Universite Paris Sud                                *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

(*
 * hashcons: hash tables for hash consing
 * Copyright (C) 2000 Jean-Christophe FILLIATRE
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(*s Hash tables for hash consing. 

    Hash consed values are of the
    following type [hash_consed]. The field [tag] contains a unique
    integer (for values hash consed with the same table). The field
    [hkey] contains the hash key of the value (without modulo) for
    possible use in other hash tables (and internally when hash
    consing tables are resized). The field [node] contains the value
    itself. 

    Hash consing tables are using weak pointers, so that values that are no
    more referenced from anywhere else can be erased by the GC. *)

type 'a hash_consed = private { 
  hkey : int;
  tag : int;
  node : 'a }

(*s Generic part, using ocaml generic equality and hash function. *)

type 'a t

val create : int -> 'a t
  (** [create n] creates an empty table of initial size [n]. The table
      will grow as needed. *)  
val clear : 'a t -> unit
  (** Removes all elements from the table. *)
val hashcons : 'a t -> 'a -> 'a hash_consed
  (** [hashcons t n] hash-cons the value [n] using table [t] i.e. returns
      any existing value in [t] equal to [n], if any; otherwise, allocates
      a new one hash-consed value of node [n] and returns it. 
      As a consequence the returned value is physically equal to
      any equal value already hash-consed using table [t]. *)
val iter : ('a hash_consed -> unit) -> 'a t -> unit
  (** [iter f t] iterates [f] over all elements of [t]. *)
val stats : 'a t -> int * int * int * int * int * int
  (** Return statistics on the table.  The numbers are, in order:
      table length, number of entries, sum of bucket lengths,
      smallest bucket length, median bucket length, biggest bucket length. *)

(*s Functorial interface. *) 

module type HashedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
  end

module type S =
  sig
    type key
    type t
    val create : int -> t
    val clear : t -> unit
    val hashcons : t -> key -> key hash_consed
    val iter : (key hash_consed -> unit) -> t -> unit
    val stats : t -> int * int * int * int * int * int
  end

module Make(H : HashedType) : (S with type key = H.t)
