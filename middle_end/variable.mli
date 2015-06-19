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

    Introducing a new type helps tracing the source of identifier
    when debugging the inliner; and, avoids ident renaming when
    importing cmx files.
*)

include Ext_types.Identifiable

val create : current_compilation_unit:Compilation_unit.t -> string -> t

val unwrap : t -> Ident.t (* For bytecode debugger only *)
val unique_ident : t -> Ident.t (* For clambdagen only *)
  (* Should we propagate Variable.t into clambda ??? *)

val rename : current_compilation_unit:Compilation_unit.t ->
  ?append:string -> t -> t

val in_compilation_unit : Compilation_unit.t -> t -> bool

val unique_name : t -> string

val output_full : out_channel -> t -> unit

val get_compilation_unit : t -> Compilation_unit.t
