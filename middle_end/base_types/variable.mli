(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** [Variable.t] is the equivalent of a non-persistent [Ident.t] in
    the [Flambda] tree.  It wraps an [Ident.t] together with its source
    [compilation_unit].  As such, it is unique within a whole program,
    not just one compilation unit.

    Introducing a new type helps in tracing the source of identifiers
    when debugging the inliner.  It also avoids Ident renaming when
    importing cmx files.
*)

include Ext_types.Identifiable

val create : ?current_compilation_unit:Compilation_unit.t -> string -> t
val create_with_same_name_as_ident : Ident.t -> t

val clambda_name : t -> string
(* CR-someday pchambart: Should we propagate Variable.t into clambda ??? *)

val rename
   : ?current_compilation_unit:Compilation_unit.t
  -> ?append:string
  -> t
  -> t

val in_compilation_unit : t -> Compilation_unit.t -> bool

val unique_name : t -> string

val get_compilation_unit : t -> Compilation_unit.t

val print_list : Format.formatter -> t list -> unit
val print_opt : Format.formatter -> t option -> unit

(** If the given variable has the given stamp, call the user-supplied
    function.  For debugging purposes only. *)
val debug_when_stamp_matches : t -> stamp:int -> f:(unit -> unit) -> unit

type pair = t * t
module Pair : Ext_types.Identifiable with type t := pair

val compare_lists : t list -> t list -> int

val output_full : out_channel -> t -> unit
(** Unlike [output], [output_full] includes the compilation unit. *)
