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
      (** A value of type ['a ttype] is a runtime representation of
          the type ['a]. Values of this type can be constructed with
          [(type ...)] expressions. *)

(** {2 Structure of types.} *)

type stype =
  | DT_arrow of string * stype * stype
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
    (** This function returns a term that represents the known part of a type structure. *)


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
    (** Checks whether two stypes are structurally equal. Abstract
        (non built-in) nodes are compared with physical equalities. *)

val equal: 'a ttype -> 'b ttype -> ('a, 'b) TypEq.t option
    (** Checks whether two ttypes have the same structure; when this is the case,
        this function returns a witness of equality between the two corresponding static types. *)

(** {2 Dynamic values.} *)

module type DYN = sig
  type t
  val x: t
  val t: t ttype
end

type dyn = (module DYN)
      (** A dynamic value is a pair of a value and of a ttype corresponding to the
          static type of the value. *)


val dyn: 'a ttype -> 'a -> dyn
    (** Build a dynamic value. *)

val dyn_tuple: dyn list -> dyn
    (** Create a tuple of dynamic values from individual components. *)


(** {2 Inspection of values.} *)

type 'a head =
  | DV_tuple of 'a list
  | DV_record of (string * 'a) list
  | DV_constructor of string * 'a list

exception AbstractValue

val inspect: dyn -> dyn head
    (** Inspect the head of the structure of a dynamic value. Raises [AbstractValue]
        for abstract and builtin nodes. *)

val build: 'a ttype -> < toval: 'b. 'b ttype -> 'b > head -> 'a
    (** Build a typed value from its ttypes and a value representing the head
        of its structure. *)

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

module DBool: TYPE0 with type t = bool


(** {2 Function types.} *)

module DArrow: sig
  module type T = sig
    type a
    type dom
    type codom
    val lab: string
    val dom: dom ttype
    val codom: codom ttype
    val eq: (a, (dom -> codom)) TypEq.t
  end

  val ttype: string -> 'a ttype -> 'b ttype -> ('a -> 'b) ttype
  val decompose: ('a -> 'b) ttype -> string * 'a ttype * 'b ttype
  val check: 'a ttype -> (module T with type a = 'a) option

  module type V = sig
    type dom
    type codom
    val lab: string
    val dom: dom ttype
    val codom: codom ttype
    val f: dom -> codom
  end
  val inspect: dyn -> (module V) option
end
