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
  | DT_abstract
  | DT_builtin

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

val stype_equality: stype -> stype -> bool
val equal: 'a ttype -> 'b ttype -> ('a, 'b) TypEq.t option

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
  | DV_tuple of dyn list
  | DV_record of (string * dyn) list
  | DV_constructor of string * dyn list

exception AbstractValue of node
val inspect: dyn -> head


(** {2 Abstract types.} *)

module type TYPE0 = sig
  type t
  val node: node
  val ttype: t ttype
  val inspect: dyn -> t option
end

module type TYPE1 = sig
  type 'a t
  module type T = sig
    type a
    type b
    val b: b ttype
    val eq: (a, b t) TypEq.t
  end

  val node: node
  val ttype: 'a ttype -> 'a t ttype
  val decompose: 'a t ttype -> 'a ttype

  val check: 'a ttype -> (module T with type a = 'a) option

  module type V = sig
    type b
    val b: b ttype
    val x: b t
  end
  val inspect: dyn -> (module V) option
end


module Abstract0(X : sig val name: string type t end) : TYPE0 with type t = X.t
module Abstract1(X : sig val name: string type 'a t end) : TYPE1 with type 'a t = 'a X.t

module DList: TYPE1 with type 'a t = 'a list
module DOption: TYPE1 with type 'a t = 'a option
module DArray: TYPE1 with type 'a t = 'a array

module DInt: TYPE0 with type t = int
module DString: TYPE0 with type t = string
module DFloat: TYPE0 with type t = float
