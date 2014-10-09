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

(** A symbol is an identifier of a constant provided by another
    compilation unit or of top level module.

    * [sym_unit] is the compilation unit containing the value.
    * [sym_label] is the linking name of the variable.

    The label must be globaly unique: two compilation units linked
    in the same program must not share labels *)


(***********************************************************************)

type linkage_name
val linkage_name : string -> linkage_name
val string_of_linkage_name : linkage_name -> string


(***********************************************************************)

module Compilation_unit : sig

  type t

  val create : string -> linkage_name -> t

  val get_persistent_ident : t -> Ident.t
  val get_linkage_name : t -> linkage_name

  include PrintableHashOrdered with type t := t

end

type compilation_unit = Compilation_unit.t

module CompilationUnitSet : ExtSet with module M := Compilation_unit
module CompilationUnitMap : ExtMap with module M := Compilation_unit
module CompilationUnitTbl : ExtHashtbl with module M := Compilation_unit


(***********************************************************************)

type t = { sym_unit : compilation_unit; sym_label : linkage_name }

module Printers : PrintableHashOrdered with type t = t

include (PrintableHashOrdered with type t := t)

module SymbolSet : ExtSet with module M := Printers
module SymbolMap : ExtMap with module M := Printers
module SymbolTbl : ExtHashtbl with module M := Printers
