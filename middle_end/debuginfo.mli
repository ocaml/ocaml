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

  val file : t -> string
  val line : t -> int
  val char_start : t -> int
  val char_end : t -> int

  include Identifiable.S with type t := t
end

module Block : sig
  type t

  val create_lexical_scope : parent:t -> t

  val create_non_inlined_frame : Code_range.t -> t

  type frame_classification = private
    | No_frame
    | Non_inlined_frame of Code_range.t
    | Inlined_frame of Code_range.t

  val frame_classification : t -> Frame_classification.t

  include Identifiable.S with type t := t
end

module Current_block : sig
  type t

  val toplevel : t

  val inline : t -> at_call_site:t -> t
end

type t
type debuginfo = t

include Identifiable.S with type t := t

val none : t

val is_none : t -> bool

val to_string_frames_only_innermost_last : t -> string

val from_location : Location.t -> scope:Current_block.t -> t

val to_location : t -> Location.t

val innermost_block : t -> Block.t option

val position : t -> Code_range.t option

val iter_innermost_first : t -> f:(Block.t -> unit) -> unit

val iter_frames_innermost_first : t -> f:(Code_range.t -> unit) -> unit

module Block_subst : sig
  type t

  val empty : t

  val find_or_add
     : t
    -> debuginfo
    -> at_call_site:Current_block.t
    -> t * debuginfo

  val find_or_add_block
     : t
    -> Block.t
    -> at_call_site:Current_block.t
    -> t * Block.t
end
