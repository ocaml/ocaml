(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Registers equipped with extra data about their contents that is used
    for generating debugging information; together with data structures
    used for keeping track of such registers. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** What a particular register holds. *)
module Holds_value_of : sig
  type t =
    | Var of Backend_var.t
      (** The value of the given variable. *)
    | Const_int of Targetint.t
      (** The given integer constant. *)
    | Const_naked_float of Int64.t
      (** The floating-point constant with the given bit pattern. *)
    | Const_symbol of String.t
      (** The given statically-allocated constant. *)

  include Identifiable.S with type t := t
end

module Debug_info : sig
  (** The type of debugging information attached to a register. *)
  type t

  (** Create a value of type [t]. *)
  val create
     : holds_value_of:Holds_value_of.t
    -> part_of_value:int
    -> num_parts_of_value:int
    -> Is_parameter.t
    -> provenance:Backend_var.Provenance.t option
    -> t

  (** Total order on values of type [t]. *)
  val compare : t -> t -> int

  (** The identifier or constant that the register holds (part of) the
      value of. *)
  val holds_value_of : t -> Holds_value_of.t

  (** If the register holds only one part of a value (for example half of a
      64-bit constant), then this function returns how many parts there are,
      or unity otherwise. *)
  val num_parts_of_value : t -> int

  (** If [num_parts_of_value] is greater than zero, then this function returns
      the zero-based index of which part of the whole value is contained within
      the register; otherwise it returns zero. *)
  val part_of_value : t -> int

  (* CR-soon mshinwell: This doesn't seem quite right, because the provenance
     is only relevant for the [Var] case.  Likewise [is_parameter].  We should
     try to improve this after the initial merge. *)

  (** A description as to whether the register holds the value of a function
      parameter or local variable.  (Anonymous constants will count as
      local variables.) *)
  val is_parameter : t -> Is_parameter.t

  (** Any provenance information, which can be used for linking back to
      .cmt files, associated with the register.  This will be [None] except
      in the [Var] case. *)
  val provenance : t -> Backend_var.Provenance.t option
end

(** Maps from registers to debug info. These can also be seen as sets of
    registers with debug info (effectively values of type [reg_with_debug_info],
    see below) subject to the condition that within any given set, any given
    register is associated with at most one debug info value. *)
module Availability_map : sig
  type t

  (** Print the given map to a formatter. *)
  val print : Format.formatter -> t -> unit

  (** Equality on maps, testing both the [Reg.t] and the [Debug_info.t option]
      components (i.e. the domain and the range). *)
  val equal : t -> t -> bool

  (** The empty map. *)
  val empty : t

  (** Create a map with a single element mapping the given register to the
      given debugging information. *)
  val singleton : Reg.t -> Debug_info.t option -> t

  (** Add a new binding to the given map or replace any existing binding
      of the same [Reg.t]. *)
  val add_or_replace : t -> Reg.t -> Debug_info.t option -> t

  (** Create a map from the given associative array.  An exception is raised
      if the input contains duplicate [Reg.t] values. *)
  val of_assoc_array : (Reg.t * (Debug_info.t option)) array -> t

  (** Test whether the given map maps the given register. *)
  val mem : t -> Reg.t -> bool

  (** Locate the debugging information associated with the given register
      in the map.  [None] being returned indicates that the register is not
      mapped in the given map.  [Some None] indicates that the register is
      mapped but has no associated debugging information. *)
  val find : t -> Reg.t -> Debug_info.t option option

  (** Return all registers mapped by the given map. *)
  val keys : t -> Reg.Set.t

  (** Map the range of the given map, i.e. the debugging information
      components. *)
  val map : t -> f:(Debug_info.t option -> Debug_info.t option) -> t

  (** Keep only the bindings in the given map for which the given
      predicate returns [true]. *)
  val filter : t -> f:(Reg.t -> bool) -> t

  (** [diff_domain t1 t2] removes bindings from [t1] whose [Reg.t] key occurs
      in the domain of [t2].  Note that this function does not look at the
      range (i.e. the [Debug_info.t option] values) of the maps. *)
  val diff_domain : t -> t -> t

  (** [inter t1 t2] returns those bindings that occur in both [t1] and [t2].
      Both the [Reg.t] and [Debug_info.t option] components of a binding have
      to be equal for such binding to be returned in the result. *)
  val inter : t -> t -> t

  (** [disjoint_union t1 t2] is the union operation on maps restricted to the
      case where the domains of [t1] and [t2] are disjoint.  If this condition
      is not satisfied a fatal error is raised. *)
  val disjoint_union : t -> t -> t

  (** [made_unavailable_by_clobber t ~regs_clobbered ~register_class] returns
      the largest subset of [t] whose locations do not overlap with any
      registers in [regs_clobbered].  (Think of [t] as a set of available
      registers.)
      [register_class] should always be [Proc.register_class]. *)
  val made_unavailable_by_clobber
     : t
    -> regs_clobbered:Reg.t array
    -> register_class:(Reg.t -> int)
    -> t
end

(** The type of a register with associated debugging information. Each value
    of type [t] holds an "underlying" [Reg.t] value. By the time this module
    is used, all such [Reg.t] values will describe hard registers or stack
    slots, and not unallocated pseudos. *)
type t

type reg_with_debug_info = t

(** Print a value of type [t] to a formatter. *)
val print
   : ?print_reg:(Format.formatter -> Reg.t -> unit)
  -> Format.formatter
  -> t
  -> unit

(** Return the normal [Reg.t] value described by the given register with
    debug info. *)
val reg : t -> Reg.t

(** Where the register is located (a hard register, the stack, etc). *)
val location : t -> Reg.location

(** The debugging information associated with the register. *)
val debug_info : t -> Debug_info.t option

(** Maps from registers to debug info, kept in a canonical form, guaranteeing
    for each map that it:
    (a) contains only registers that are associated with debug info; and
    (b) contains at most one register that holds the value of any given
        variable.

    Registers assigned to the stack are preferred if a choice has to be
    made to satisfy (b).
*)
module Canonical_availability_map : sig
  type t

  (** Print the given map to a formatter. *)
  val print : Format.formatter -> t -> unit

  (** The empty map. *)
  val empty : t

  (** Canonicalise the given register-to-debug-info map. *)
  val create : Availability_map.t -> t

  (** [of_list] will raise an exception if more than one entry in the
      supplied list contains the same [Reg.t]. *)
  val of_list : reg_with_debug_info list -> t

  (** Returns [true] iff the supplied map does not contain any bindings. *)
  val is_empty : t -> bool

  (** Note that no "union" operations are provided.  Values of type [t]
      form a semilattice with respect to [inter], but not a lattice also
      with respect to [union], since the canonical form might not be
      preserved. *)

  (** [diff t1 t2] returns those bindings in [t1] that do not occur in [t2].
      Note that both the [Reg.t] and the [Debug_info.t] components are
      taken into account.  (This means, for example, that if [t1] contains
      a mapping of register [r] to debug info [di1] and if [t2] contains a
      mapping of register [r] to debug info [di2] with
      [not (Debug_info.equal di1 di2)] then [diff t1 t2] returns [t1]
      unchanged.) *)
  val diff : t -> t -> t

  (** [inter t1 t2] returns those bindings that occur in both [t1] and [t2].
      As for [diff], both the [Reg.t] and the [Debug_info.t] components of
      each binding are considered, and must match for a binding to be
      returned in the intersection. *)
  val inter : t -> t -> t

  (** Fold over the bindings in the given map; the order is unspecified. *)
  val fold : (reg_with_debug_info -> 'a -> 'a) -> t -> 'a -> 'a

  (** Find the element of the set that holds the value of the given variable,
      if such exists, otherwise returning [None].  (Note that by virtue of the
      canonical form criterion, there can never be more than one variable
      eligible to be returned from any one call to this function.) *)
  val find_holding_value_of_variable
     : t
    -> Backend_var.t
    -> reg_with_debug_info option
end

(** Convenience module for use with [Compute_ranges_intf]. *)
module For_compute_ranges : sig
  type nonrec t = t

  val print : Format.formatter -> t -> unit

  module Set = Canonical_availability_map
  module Map : Map.S with type key = t
end
