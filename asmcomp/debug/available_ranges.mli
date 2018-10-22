(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** For registers:

    Given a variable [x] and a function, an "available subrange" is in the
    normal case a contiguous subset of that function's code paired with a
    register [r], such that at all times during the block's execution the value
    of [x] is available in [r]. ([r] may end up being a hard register or a
    location on the stack.)
 
    An available subrange may instead be associated with a phantom variable.
    Phantom variables correspond to variables that once bound computations that
    have now been optimised out.
 
    Available subranges associated with normal variables are computed by this
    pass based on the information from the dataflow analysis in
    [Available_regs]. (The linearized code is updated so that it contains the
    necessary labels for delimiting such ranges.) Those associated with phantom
    variables, however, are tracked explicitly from the [Uphantom_let] Clambda
    expression onwards.
 
    An "available range" is then a set of available subranges that do not
    overlap in code space, again for a single variable (normal or phantom) and
    function.

    For lexical blocks/scopes:

    An analogous concept, except that we compute ranges of code that
    correspond to lexical scopes.  Each computed range, which consists of a
    number of subranges as in the registers case above, is linked back to the
    [Debuginfo.Block] it refers to.  That same information can also be
    obtained from [Reg_with_debug_info.t] values, which means that given a
    register in an availability set, it is possible to match it up with the
    corresponding available range.  Hence we can assign DWARF lexical scopes
    based on the available range information and then put variables in the
    appropriate scopes.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Available_subrange_info_for_regs : sig
  type t

  type type_info = private
    | From_cmt_file of Backend_var.Provenance.t option
    | Phantom of
        Backend_var.Provenance.t option * Mach.phantom_defining_expr option

  type is_parameter = private
    | Local
    | Parameter of { index : int; }

  val type_info : t -> type_info
  val is_parameter : t -> is_parameter
end

module Available_subrange_info_for_blocks : sig
  type t
end

module Available_range_info_for_regs : sig
  type t

  type 'a location = private
    | Reg of Reg.t * 'a
    | Phantom

  val lexical_scope : t -> Debuginfo.Block.t

  val location : t -> unit location

  (** [offset_from_stack_ptr_in_bytes] must be called only when [location]
      is [Reg] and the contained register is assigned to the stack. *)
  val offset_from_stack_ptr_in_bytes : t -> int
end

type range_uniqueness_for_regs = private {
  name_is_unique : bool;
  location_is_unique : bool;
}

module Available_range_info_for_blocks : sig
  type t
end

module type S = sig
  type subrange_info
  type range_info
  type range_uniqueness
  type range_index

  module Available_subrange : sig
    type t

    val info : t -> subrange_info

    val start_pos : t -> Linearize.label
    val end_pos : t -> Linearize.label
    val end_pos_offset : t -> int option
  end

  module Available_range : sig
    type t

    val info : t -> range_info

    val extremities : t -> Linearize.label * Linearize.label

    val iter
       : t
      -> f:(available_subrange:Available_subrange.t -> unit)
      -> unit

    val fold
       : t
      -> init:'a
      -> f:('a -> available_subrange:Available_subrange.t -> 'a)
      -> 'a
  end

  type t

  (** [create fundecl] may change [fundecl].  It may change the first
      instruction, even, which is why a new declaration is returned. *)
  val create : Linearize.fundecl -> t * Linearize.fundecl

  val find : t -> range_index -> Available_range.t option

  val fold
     : t
    -> init:'a
    -> f:('a
      -> range_index
      -> range_uniqueness
      -> Available_range.t
      -> 'a)
    -> 'a
end

module Regs : S
  with type subrange_info := Available_subrange_info_for_regs.t
  with type range_info := Available_range_info_for_regs.t
  with type range_index := Backend_var.t
  with type range_uniqueness := range_uniqueness_for_regs

module Phantom_vars : S
  with type subrange_info := Available_subrange_info_for_regs.t
  with type range_info := Available_range_info_for_regs.t
  with type range_index := Backend_var.t
  with type range_uniqueness := range_uniqueness_for_regs

module Lexical_blocks : S
  with type subrange_info := Available_subrange_info_for_blocks.t
  with type range_info := Available_range_info_for_blocks.t
  with type range_index := Debuginfo.Block.t
  with type range_uniqueness := unit
