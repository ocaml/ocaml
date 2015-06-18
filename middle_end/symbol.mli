(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Ext_types

(* CR mshinwell for pchambart: I think maybe this module should only provide
   the [Symbol] stuff, and we should move the other things elsewhere.
   In particular, [Symbol.compilation_unit] doesn't make much sense. *)

(** A symbol is an identifier of a constant provided by another
    compilation unit or of top level module.

    * [sym_unit] is the compilation unit containing the value.
    * [sym_label] is the linking name of the variable.

    The label must be globaly unique: two compilation units linked
    in the same program must not share labels *)


(***********************************************************************)

(* CR mshinwell for pchambart: [module Linkage_name] *)
type linkage_name
val linkage_name : string -> linkage_name
val string_of_linkage_name : linkage_name -> string


(***********************************************************************)

(* CR mshinwell: this deserves its own source file *)
module Compilation_unit : sig

  type t
  include Identifiable with type t := t

  val create : Ident.t -> linkage_name -> t

  val get_persistent_ident : t -> Ident.t
  val get_linkage_name : t -> linkage_name

  val set_current : t -> unit
  val get_current : unit -> t option
  val get_current_exn : unit -> t
  val get_current_id_exn : unit -> Ident.t
end

(***********************************************************************)

type t = { sym_unit : Compilation_unit.t; sym_label : linkage_name }

module Printers : PrintableHashOrdered with type t = t

include (PrintableHashOrdered with type t := t)

module Symbol_Identifiable : Identifiable
  with type t = t

module SymbolSet = Symbol_Identifiable.Set
module SymbolMap = Symbol_Identifiable.Map
module SymbolTbl = Symbol_Identifiable.Tbl

module ExportId : UnitId with module Compilation_unit := Compilation_unit
