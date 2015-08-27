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

(** Exported information (that is to say, information written into a .cmx
    file) about a compilation unit. *)

type value_string_contents =
  | Contents of string
  | Unknown_or_mutable

type value_string = {
  contents : value_string_contents;
  size : int;
}

type descr =
  | Value_block of Tag.t * approx array
  | Value_mutable_block of Tag.t * int
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_float_array of int
  | Value_boxed_int : 'a Simple_value_approx.boxed_int * 'a -> descr
  | Value_string of value_string
  | Value_closure of value_closure
  | Value_set_of_closures of value_set_of_closures

and value_closure = {
  closure_id : Closure_id.t;
  set_of_closures : value_set_of_closures;
}

and value_set_of_closures = {
  set_of_closures_id : Set_of_closures_id.t;
  bound_vars : approx Var_within_closure.Map.t;
  results : approx Closure_id.Map.t;
}

and approx =
  | Value_unknown
  | Value_id of Export_id.t
  | Value_symbol of Symbol.t

type exported = {
  (* Code of exported functions indexed by function identifier *)
  functions : Flambda.function_declarations Set_of_closures_id.Map.t;
  (* Code of exported functions indexed by offset identifier *)
  functions_off : Flambda.function_declarations Closure_id.Map.t;
  (* Structure of exported values  *)
  values : descr Export_id.Map.t Compilation_unit.Map.t;
  (* Global variables provided by the unit: usualy only the top-level
     module identifier, but packs contains multiple ones. *)
  globals : approx Ident.Map.t;
  id_symbol : Symbol.t Export_id.Map.t Compilation_unit.Map.t;
  (* Associates symbols and values *)
  symbol_id : Export_id.t Symbol.Map.t;
  (* Positions of function pointers in their closures *)
  offset_fun : int Closure_id.Map.t;
  (* Positions of value pointers in their closures *)
  offset_fv : int Var_within_closure.Map.t;
  (* Symbols that are effectively constants (the top-level module is
     not always a constant for instance) *)
  constants : Symbol.Set.t;
  constant_closures : Set_of_closures_id.Set.t;
  invariant_arguments : Variable.Set.t Set_of_closures_id.Map.t;
}

val empty_export : exported

(** Union of export informations. Verify that there is no identifier
    clash. *)
val merge
   : exported
  -> exported
  -> exported

(** Transform the information from [exported] to be
    suitable to be reexported as the information for a pack named [pack]
    containing units [pack_units].
    It mainly changes symbols of units [pack_units] to refer to
    [pack] instead. *)
val import_for_pack
   : pack_units:Compilation_unit.Set.t
  -> pack:Compilation_unit.t
  -> exported
  -> exported

(** Drops the state after importing several units in the same pack. *)
val clear_import_state : unit -> unit

val find_description
   : Export_id.t
  -> exported
  -> descr

val nest_eid_map
   : 'a Export_id.Map.t
  -> 'a Export_id.Map.t Compilation_unit.Map.t

(**/**)
(* debug printing functions *)

val print_approx : Format.formatter -> exported -> unit

val print_symbols : Format.formatter -> exported -> unit

val print_offsets : Format.formatter -> exported -> unit

val print_all : Format.formatter -> exported -> unit
