(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                      Pierre Chambart (OCamlPro)                        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Exported informations about a compilation unit *)

open Ext_types
open Symbol
open Abstract_identifiers

module ExportId : UnitId with module Compilation_unit := Compilation_unit
module EidSet : ExtSet with module M := ExportId
module EidMap : ExtMap with module M := ExportId
module EidTbl : ExtHashtbl with module M := ExportId

type tag = int

type descr =
    Value_block of tag * approx array
  | Value_int of int
  | Value_constptr of int
  | Value_closure of value_offset
  | Value_unoffseted_closure of value_closure

and value_offset =
  { fun_id : function_within_closure;
    closure : value_closure; }

and value_closure =
  { closure_id : FunId.t;
    bound_var : approx ClosureVariableMap.t;
    results : approx ClosureFunctionMap.t }

and approx =
    Value_unknown
  | Value_id of ExportId.t
  | Value_symbol of Symbol.t

type exported = {
  ex_functions : unit Flambda.function_declarations FunMap.t;
  (** Code of exported functions indexed by function identifier *)
  ex_functions_off : unit Flambda.function_declarations ClosureFunctionMap.t;
  (** Code of exported functions indexed by offset identifier *)
  ex_values : descr EidMap.t;
  (** Structure of exported values  *)
  ex_globals : approx IdentMap.t;
  (** Global variables provided by the unit: usualy only the top-level
      module identifier, but packs contains multiple ones. *)

  ex_id_symbol : Symbol.t EidMap.t;
  ex_symbol_id : ExportId.t SymbolMap.t;
  (** Associates symbols and values *)

  ex_offset_fun : int ClosureFunctionMap.t;
  (** Positions of function pointers in their closures *)
  ex_offset_fv : int ClosureVariableMap.t;
  (** Positions of value pointers in their closures *)
  ex_constants : SymbolSet.t;
  (** Symbols that are effectively constants (the top-level module is
      not always a constant for instance) *)
  ex_constant_closures : FunSet.t;
  ex_kept_arguments : VarSet.t FunMap.t;
}

val empty_export : exported

val merge : exported -> exported -> exported
(** Union of export informations. Verify that there is no identifier
    clash. *)

val import_for_pack :
  pack_units:CompilationUnitSet.t -> pack:compilation_unit -> exported -> exported
(** Transform the informations from [exported] to be suitable to
    be reexported as the informations for a pack named [pack]
    containing units [pack_units].
    It mainly change symbols of units [pack_units] to refer to
    [pack] instead. *)

(**/**)
(* debug printing functions *)

val print_approx : Format.formatter -> exported -> unit

val print_symbols : Format.formatter -> exported -> unit

val print_all : Format.formatter -> exported -> unit
