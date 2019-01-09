(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*             Mark Shinwell and Leo White, Jane Street Europe            *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Code_range : sig
  type t

  val create : file:string -> line:int -> char_start:int -> char_end:int -> t

  val none : t

  val file : t -> string
  val line : t -> int
  val char_start : t -> int
  val char_end : t -> int

  val print_compact : Format.formatter -> t -> unit

  include Identifiable.S with type t := t

  module Option : sig
    type nonrec t = t option

    include Identifiable.S with type t := t
  end
end

(* CR mshinwell: Locations may be off by one line for inlined bodies *)

module Function : sig
  (** A value of type [t] uniquely identifies a function within a whole
      program.  It also provides information about the source location of
      the function, its name, and so forth.

      Unique identifiers of functions are necessary for emitting DWARF debugging
      information: in particular they are used to match up inlined frames
      occurring in [Debuginfo.t] values with the corresponding debugging
      information entries for the relevant functions. We do not use symbols as
      unique identifiers since in Flambda they may not have been assigned before
      a debuginfo value is required. *)

  type t

  (** Create a function debuginfo given a [Code_range.t] corresponding to
      its source location. *)
  val create
     : Code_range.t
    -> human_name:string
    -> module_path:Path.t
    -> linkage_name:Linkage_name.t
    -> t

  (** Create a function debuginfo given a [Location.t] corresponding to
      its source location. *)
  val create_from_location
     : Location.t
    -> human_name:string
    -> module_path:Path.t
    -> linkage_name:Linkage_name.t
    -> t

  (** Create a function debuginfo given a filename and line number
      corresponding to its source location. *)
  val create_from_line
     : file:string
    -> line:int
    -> human_name:string
    -> module_path:Path.t
    -> linkage_name:Linkage_name.t
    -> t

  (** Update the source code position of the given function debuginfo. *)
  val with_position : t -> Code_range.t -> t

  module Id : sig
    (** The actual unique identifier for a function. *)
    type t

    (** Which compilation unit the function was defined in. *)
    val compilation_unit : t -> Compilation_unit.t

    include Identifiable.S with type t := t

    (** A textual representation suitable for including within symbol
        names of DWARF debugging information entries. *)
    val to_string_for_dwarf_die_name : t -> string
  end

  (** The unique identifier for the function. *)
  val id : t -> Id.t

  (** The source code location of the function. *)
  val position : t -> Code_range.t

  (** The name to be displayed in a debugger to identify the function,
      without any module path qualification. *)
  val human_name : t -> string

  (** The linkage name of the function. *)
  val linkage_name : t -> Linkage_name.t

  (** The module path to the function. *)
  val module_path : t -> Path.t

  (* CR mshinwell: Rename this and/or [human_name]. *)
  (** The name to be displayed in a debugger to identify the function,
      including any module path qualification. *)
  val name : t -> string

  (** Whether the function is visible outside the compilation unit in which
      it is defined. *)
  val is_visible_externally : t -> bool

  (** Whether a DWARF debugging information entry should be expected to be
      present for this function.  Such entries are only expected when full
      DWARF information is being emitted.  This function is used to prevent
      the emission of references to unbound symbols (purportedly referencing
      DWARF DIEs) when part of a program is compiled without full DWARF
      debugging information enabled. *)
  val dwarf_die_present : t -> bool

  (** Comparison, equality, hashing and printing. *)
  include Identifiable.S with type t := t

  (** Compare two function debuginfo values only on their source location. *)
  val compare_on_source_position_only : t -> t -> int
end

module Call_site : sig
  type t

  val create_from_location : Function.t -> Location.t -> t

  val fun_dbg : t -> Function.t
  val position : t -> Code_range.t

  val print : Format.formatter -> t -> unit
end

module Block : sig
  (** A "block" represents the source-level structures within which a
      particular computation is nested.  The following varieties of
      structures are tracked:

      - Lexical blocks
      - Inlined functions (thought of as inlined-out stack frames).
  *)

  module Id : sig
    (** The actual unique identifier for a block. *)
    type t

    (** Which compilation unit the block was defined in. *)
    val compilation_unit : t -> Compilation_unit.t

    include Identifiable.S with type t := t
  end

  type t

  val create_non_inlined_frame : Call_site.t -> t

  type frame_classification = private
    | Lexical_scope_only
    | Inlined_frame of Call_site.t

  val frame_classification : t -> frame_classification

  val parent : t -> t option

  val parents_transitive : t -> t list

  val unique_id : t -> Id.t

  include Identifiable.S with type t := t

  val print_id : Format.formatter -> t -> unit
end

module Current_block : sig
  type t

  val toplevel : t

  type to_block =
    | Toplevel
    | Block of Block.t

  val to_block : t -> to_block

  val add_scope : t -> t

  val add_inlined_frame : t -> Call_site.t -> t

  include Identifiable.S with type t := t
end

type t
type debuginfo = t

include Identifiable.S with type t := t

(* CR-soon mshinwell: Remove [none] and [is_none]. *)

val none : t

val is_none : t -> bool

val to_string_frames_only_innermost_last : t -> string

val of_line : file:string -> line:int -> scope:Current_block.t -> t

val of_location : Location.t -> scope:Current_block.t -> t

val to_location : t -> Location.t

val of_function : Function.t -> t

val innermost_block : t -> Current_block.t

val with_position : t -> Code_range.t -> t

val position : t -> Code_range.t option

val iter_position_and_blocks_innermost_first
   : t
  -> f_position:(Code_range.t -> unit)
  -> f_blocks:(Block.t -> unit)
  -> unit

val iter_position_and_frames_innermost_first
   : t
  -> f:(Code_range.t -> unit)
  -> unit

val best : t -> t -> t

module Apply : sig
  type t

  val create : Function.t -> debuginfo -> t

  val fun_dbg : t -> Function.t
  val dbg : t -> debuginfo
end

module Block_subst : sig
  type t

  val empty : t

  val find_or_add
     : t
    -> debuginfo
    -> at_call_site:Current_block.t
    -> t * debuginfo

  (** This function does not affect the [Function.t] component of the
      [Apply.t] debug info. *)
  val find_or_add_for_apply
     : t
    -> Apply.t
    -> at_call_site:Current_block.t
    -> t * Apply.t
end
