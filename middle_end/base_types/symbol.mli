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

(** A symbol identifies a constant provided by either:
    - another compilation unit; or
    - a top-level module.

    * [sym_unit] is the compilation unit containing the value.
    * [sym_label] is the linkage name of the variable.

    The label must be globally unique: two compilation units linked in the
    same program must not share labels. *)

include Ext_types.Identifiable

val create : Compilation_unit.t -> Linkage_name.t -> t

val compilation_unit : t -> Compilation_unit.t
val label : t -> Linkage_name.t
