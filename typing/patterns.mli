(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Gabriel Scherer, projet Partout, INRIA Paris-Saclay           *)
(*          Thomas Refis, Jane Street Europe                              *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Typedtree
open Types

val omega : pattern
(** aka. "Tpat_any" or "_"  *)

val omegas : int -> pattern list
(** [List.init (fun _ -> omega)] *)

val omega_list : 'a list -> pattern list
(** [List.map (fun _ -> omega)] *)

module Non_empty_row : sig
  type 'a t = 'a * Typedtree.pattern list

  val of_initial : Typedtree.pattern list -> Typedtree.pattern t
  (** 'assert false' on empty rows *)

  val map_first : ('a -> 'b) -> 'a t -> 'b t
end

module Simple : sig

  type core_view = [
    | `Any
    | `Constant of constant
    | `Tuple of pattern list
    | `Construct of
        Longident.t loc * constructor_description * pattern list
    | `Variant of label * pattern option * row_desc ref
    | `Array of pattern list
    | `Lazy of pattern
  ]

  type record_view = [
      `Record of
        (Longident.t loc * label_description * pattern) list * closed_flag
  ]

  type expanded_record_view = [
      `Expanded_record of
        (Longident.t loc * label_description * pattern) list
  ]

  type view = [ core_view | expanded_record_view ]
  type nonexpanded_view = [ view | record_view ]
  type pattern = view pattern_data
  type nonexpanded_pattern = nonexpanded_view pattern_data

  val omega : [> `Any ] pattern_data

end

module Half_simple : sig
  type view = [
    | Simple.view
    | `Or of pattern * pattern * row_desc option
  ]

  type nonexpanded_view = [
    | Simple.nonexpanded_view
    | `Or of pattern * pattern * row_desc option
  ]

  type pattern = view pattern_data
  type nonexpanded_pattern = nonexpanded_view pattern_data

end

module General : sig
  type view = [
    | Half_simple.view
    | Simple.record_view
    | `Var of Ident.t * string loc
    | `Alias of pattern * Ident.t * string loc
  ]
  type pattern = view pattern_data

  val view : Typedtree.pattern -> pattern
  val erase : [< view ] pattern_data -> Typedtree.pattern

  val strip_vars : pattern -> Half_simple.nonexpanded_pattern
end

module Head : sig
  type desc =
    | Any
    | Construct of constructor_description
    | Constant of constant
    | Tuple of int
    | Record of label_description list
    | Variant of
        { tag: label; has_arg: bool;
          cstr_row: row_desc ref;
          type_row : unit -> row_desc; }
          (* the row of the type may evolve if [close_variant] is called,
             hence the (unit -> ...) delay *)
    | Array of int
    | Lazy

  type t = desc pattern_data

  type expanded = private t

  val arity : expanded -> int
  val simple_to_head: Simple.pattern -> expanded
  val desc: expanded -> desc

  (** [deconstruct p] returns the head of [p] and the list of sub patterns.

      @raises [Invalid_arg _] if [p] is an or- or an exception-pattern.  *)
  val nonexpanded_deconstruct :
    Simple.nonexpanded_view pattern_data -> t * pattern list
  val deconstruct : Simple.view pattern_data -> expanded * pattern list

  (** reconstructs a pattern, putting wildcards as sub-patterns. *)
  val to_omega_pattern : t -> pattern

  val omega : expanded

end
