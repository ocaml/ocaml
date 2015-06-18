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

module EidSet : ExtSet with module M := ExportId
module EidMap : ExtMap with module M := ExportId
module EidTbl : ExtHashtbl with module M := ExportId

type tag = int

type 'a boxed_int = 'a Simple_value_approx.boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type value_string = Simple_value_approx.value_string = {
  contents : string option; (* None if unknown or mutable *)
  size : int;
}

type descr =
  | Value_block of tag * approx array
  | Value_mutable_block of tag * int
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_float_array of int
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_string of value_string
  | Value_closure of value_offset
  | Value_set_of_closures of value_closure

and value_offset =
  { fun_id : Closure_id.t;
    closure : value_closure; }

and value_closure =
  { closure_id : Set_of_closures_id.t;
    bound_var : approx Var_within_closure.Map.t;
    results : approx Closure_id.Map.t }

and approx =
    Value_unknown
  | Value_id of ExportId.t
  | Value_symbol of Symbol.t

type exported = {
  ex_functions : unit Flambda.function_declarations Set_of_closures_id.Map.t;
  (** Code of exported functions indexed by function identifier *)
  ex_functions_off : unit Flambda.function_declarations Closure_id.Map.t;
  (** Code of exported functions indexed by offset identifier *)
  ex_values : descr EidMap.t Compilation_unit.Map.t;
  (** Structure of exported values  *)
  ex_globals : approx Ident.Map.t;
  (** Global variables provided by the unit: usualy only the top-level
      module identifier, but packs contains multiple ones. *)

  ex_id_symbol : Symbol.t EidMap.t Compilation_unit.Map.t;
  ex_symbol_id : ExportId.t SymbolMap.t;
  (** Associates symbols and values *)

  ex_offset_fun : int Closure_id.Map.t;
  (** Positions of function pointers in their closures *)
  ex_offset_fv : int Var_within_closure.Map.t;
  (** Positions of value pointers in their closures *)
  ex_constants : SymbolSet.t;
  (** Symbols that are effectively constants (the top-level module is
      not always a constant for instance) *)
  ex_constant_closures : Set_of_closures_id.Set.t;
  ex_kept_arguments : Variable.Set.t Set_of_closures_id.Map.t;
}

val empty_export : exported

val merge : exported -> exported -> exported
(** Union of export informations. Verify that there is no identifier
    clash. *)

val import_for_pack :
  pack_units:Compilation_unit.Set.t -> pack:Compilation_unit.t -> exported -> exported
(** Transform the informations from [exported] to be suitable to
    be reexported as the informations for a pack named [pack]
    containing units [pack_units].
    It mainly change symbols of units [pack_units] to refer to
    [pack] instead. *)

val clear_import_state : unit -> unit
(** Drops the state after importing several units in the same pack. *)

val find_description : ExportId.t -> exported -> descr

val nest_eid_map : 'a EidMap.t -> 'a EidMap.t Compilation_unit.Map.t

(**/**)
(* debug printing functions *)

val print_approx : Format.formatter -> exported -> unit

val print_symbols : Format.formatter -> exported -> unit

val print_offsets : Format.formatter -> exported -> unit

val print_all : Format.formatter -> exported -> unit
