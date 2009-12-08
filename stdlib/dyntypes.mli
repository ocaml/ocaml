(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Dynamic representation of types. *)


type 'a ttype

(** {2 Structure of types.} *)

type stype =
  | DT_int
  | DT_string
  | DT_float
  | DT_tuple of stype list
  | DT_node of node * stype list
  | DT_var of int

and record_representation =
  | Record_regular
  | Record_float

and mutable_flag =
  | Mutable
  | Immutable

and node = {
    node_id: string;
    node_definition: node_definition;
   }

and node_definition =
  | DT_record of record_definition
  | DT_variant of variant_definition

and record_definition = {
    record_representation:  record_representation;
    record_fields: (string * mutable_flag * stype) list;
   }

and variant_definition = {
    variant_constructors: (string * stype list) list;
   }


val stype_of_ttype: 'a ttype -> stype

(** {2 Equality.} *)

module TypEq : sig
  type ('a, 'b) t
        (** A value of type [('a, 'b) t] is a witness that the two types ['a] and ['b] are equal. *)

  val refl: ('a, 'a) t
  val trans: ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val sym: ('a, 'b) t -> ('b, 'a) t
  val app: ('a, 'b) t -> 'a -> 'b
end

val stype_equality: unit -> stype -> stype -> bool
val equal: unit -> 'a ttype -> 'b ttype -> ('a, 'b) TypEq.t option

(** {2 Dynamic values.} *)

module type DYN = sig
  type t
  val x: t
  val t: t ttype
end

type dyn = (module DYN)

val dyn: 'a ttype -> 'a -> dyn
val tuple: dyn list -> dyn

(** {2 Inspection of values.} *)

type head =
  | DV_int of int
  | DV_string of string
  | DV_float of float
  | DV_tuple of dyn list
  | DV_record of (string * dyn) list
  | DV_constructor of string * dyn list

val inspect: dyn -> head
