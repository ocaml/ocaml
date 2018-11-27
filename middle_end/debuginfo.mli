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

(* CR-someday mshinwell: Consider introducing the notion of a "function
   block". *)

module Block : sig
  type t

  val create_non_inlined_frame : Code_range.t -> t

  type frame_classification = private
    | Lexical_scope_only
    | Non_inlined_frame of Code_range.t
    | Inlined_frame of Code_range.t

  val frame_classification : t -> frame_classification

  val parent : t -> t option

  val parents_transitive : t -> t list

  val unique_id : t -> int

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

  val inline : t -> at_call_site:t -> t

  include Identifiable.S with type t := t
end

type t
type debuginfo = t

include Identifiable.S with type t := t

(* CR-soon mshinwell: Remove [none] and [is_none]. *)

val none : t

val is_none : t -> bool

val to_string_frames_only_innermost_last : t -> string

val of_location : Location.t -> scope:Current_block.t -> t

val to_location : t -> Location.t

val innermost_block : t -> Current_block.t

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

module Block_subst : sig
  type t

  val empty : t

  val find_or_add_block
     : t
    -> Block.t
    -> at_call_site:Current_block.t
    -> t * Block.t

  val find_or_add
     : t
    -> debuginfo
    -> at_call_site:Current_block.t
    -> t * debuginfo
end
